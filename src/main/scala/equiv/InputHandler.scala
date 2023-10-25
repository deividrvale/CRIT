package equiv

import equiv.ri.{CALCULATION_SIMP, CALCULATION_VARREPL, Equation, ProofState}
import equiv.ri.Equation.Side
import equiv.ri.inference_rules.{COMPLETENESS, CONSTRUCTOR, DELETION, DISPROVE, EQ_DELETION, EXPANSION, GENERALIZATION, POSTULATE, SIMPLIFICATION}
import equiv.trs.Term.Position
import equiv.trs.parsing.{QuasiQuery, QuasiQueryEquivalence, QuasiRule, QuasiSignature, QuasiSystem, QuasiTerm, TRSParser}
import equiv.trs.{FunctionSymbol, Rule, Signature, Sort, Term, Typing}
import equiv.utils.OptionExtension.printRedOnNone
import equiv.utils.{PrintUtils, TermUtils, Z3}

import java.io.StringReader
import java.lang.System.err
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.util.control.Breaks.break

object InputHandler {
  var errorMessage: String = ""
  var endMessage: String = PrintUtils.successColourString("Rewriting induction complete. All equations deleted.")

  val inferenceRules: List[String] = List(
    COMPLETENESS.name,
    CONSTRUCTOR.name,
    DELETION.name,
    DISPROVE.name,
    EQ_DELETION.name,
    EXPANSION.name,
    GENERALIZATION.name,
    POSTULATE.name,
    SIMPLIFICATION.name,
    CALCULATION_SIMP.name,
    CALCULATION_VARREPL.name)

  def main(system: trs.System, initialPfSt: ProofState): Unit = {
    var pfSt = initialPfSt
    while pfSt.equations.nonEmpty do
      errorMessage = "Could not find equations subject to this inference rule."
      println(pfSt.toPrintString())
      val maybePfStMessage = doRIIteration(system, pfSt)
      maybePfStMessage._1 match {
        case None => println(PrintUtils.failureColourString(errorMessage))
        case Some(newPfSt) => pfSt = newPfSt
      }
      println(maybePfStMessage._2)
    println(endMessage)
  }

  /** Do an iteration of the rewriting induction process.
   * Let the user choose an inference rule (or Z3 simplification) to apply.
   * @return 1st tuple element: [[Some]]([[ProofState]]) on success and [[None]] on failure.
   * 2nd tuple element: message to print after application. */
  def doRIIteration(system: trs.System, pfSt: ProofState): (Option[ProofState], String) = {
    var message = ""
    (inferenceRuleSelector(inferenceRules) match {
      case CONSTRUCTOR.name =>
        CONSTRUCTOR.tryCONSTRUCTOR(pfSt, equationSelector)
      case COMPLETENESS.name =>
        COMPLETENESS.tryCOMPLETENESS(pfSt)
      case DELETION.name =>
        DELETION.tryDELETION(pfSt, equationSelector)
      case DISPROVE.name =>
        if (!pfSt.getFlag) { errorMessage = "ProofState is incomplete."; None } else {
          DISPROVE.tryDISPROVE(pfSt).map(_ => {
            endMessage = PrintUtils.successColourString("DISPROVE. Rewriting Induction terminated.")
            pfSt.removeAllEquations()
          })
        }
      case EQ_DELETION.name =>
        EQ_DELETION.tryEQ_DELETION(pfSt, equationSelector, positionsSelector)
      case EXPANSION.name =>
        EXPANSION.tryEXPANSION(pfSt, equationSelector, sideSelector, positionSelector, ruleAcceptor)
      case GENERALIZATION.name =>
        errorMessage = "GENERALIZATION is not implemented"
        None
      case POSTULATE.name =>
        Some(POSTULATE.doPOSTULATE(pfSt, equationsInputter(system)))
      case SIMPLIFICATION.name =>
        SIMPLIFICATION.trySIMPLIFICATION(pfSt, equationSelector, sideSelector, ruleSelector, positionSelector)
      case CALCULATION_SIMP.name =>
        Some(CALCULATION_SIMP.SIMPLIFY_CALC(pfSt))
      case CALCULATION_VARREPL.name =>
        CALCULATION_VARREPL.trySubtermVarReplacement(pfSt, equationSelector)
    }, message)
  }

  def ruleAcceptor(rule: Rule): Boolean = {
    println(rule.toPrintString())
    print("Do you want to add this rule to the hypotheses set? (Y/n): ")
    loopForCorrectLowerCaseInput(List("y", "", "yes", "no", "n")) match {
      case "" | "y" | "yes" => return true
      case "n" | "no" => return false
    }
    false
  }

  def loopForCorrectLowerCaseInput(correctInput: List[String]): String = {
    loopForInputCondition({s => correctInput.contains(s.toLowerCase)}).toLowerCase
  }

  /** Ask the user for an input line (``readLine()``) until the given condition is satisfied. */
  def loopForInputCondition(condition: String => Boolean, errorMessage: String = "Incorrect input, try again: "): String = {
    var input = readLine()
    while !condition(input) do
      print(errorMessage)
      input = readLine()
    input
  }

  /** @return A sorted [[Map]] from integers (as strings) to objects of type [[T]] */
  def withIndex[T](input: List[T]): Map[String, T] = ListMap(input.zipWithIndex.sortBy(t => t._2).map((data, i) => (i.toString, data)): _*)

  /** Prompt the user to choose a value from a list.
   * @param input The list to choose an option from
   * @param itemToString Function that transforms the list items to how they should be displayed to the user
   * @return An item from the given list */
  def selectFromList[T](input: List[T], itemToString: T => String): T = {
    val listWithIndex = withIndex(input)
    listWithIndex.foreach((i, d) => println(s"$i: ${itemToString(d)}"))
    listWithIndex(loopForCorrectLowerCaseInput(listWithIndex.keys.toList))
  }

  def inferenceRuleSelector(inferenceRules: List[String] = this.inferenceRules): String = {
    println("Choose an inference rule:")
    selectFromList(inferenceRules, identity)
  }

  def equationSelector(equations: List[Equation]): Equation = {
    println("Choose an equation:")
    selectFromList(equations, _.toPrintString())
  }

  def ruleSelector(rules: List[Rule]): Rule = {
    println("Choose a rule:")
    selectFromList(rules, _.toPrintString())
  }

  def subtermSelector(term: Term, positions: List[Position]): Position = {
    println("Choose a subterm:")
    selectFromList(positions, p => term.subTermAt(p).toPrintString())
  }

  def positionSelector(terms: Iterable[Term], positions: List[Position]): Position = {
    println("Choose a position:")
    selectFromList(positions, p => s"${Term.positionToString(p)}: ${terms.map(_.subTermAt(p).toPrintString()).mkString("", s" ${TermUtils.RIEqualityFunctionSymbolName} ", "")}" )
  }

  def positionsSelector(terms: Iterable[Term], positions: List[Position]): List[Position] = {
    var selectedPositions: Set[Position] = Set()
    var remainingPositions = positions.toSet
    var morePositions = true
    while morePositions && remainingPositions.nonEmpty do {
      val selectedPosition = positionSelector(terms, remainingPositions.toList)
      selectedPositions += selectedPosition
      remainingPositions -= selectedPosition
      if remainingPositions.nonEmpty then {
        print("Do you want to select another position? (Y/n): ")
        loopForCorrectLowerCaseInput(List("y", "yes", "", "n", "no")) match {
          case "n" | "N" => morePositions = false
          case _ =>
        }
      }
    }
    selectedPositions.toList
  }

  def sideSelector(sides: List[Side]): Side = {
    println("Choose a side:")
    selectFromList(sides, { case Side.Left => "Left"; case Side.Right => "Right" })
  }

  def equationSideSelector(equation: Equation): Side = {
    println("Choose a side:")
    selectFromList(List(Side.Left, Side.Right), side => equation.getSide(side).toPrintString())
  }

  def equationInputter(system: trs.System): Equation = {
    println("Enter an equation:")
    errorMessage = "Failed to parse equation. Please try again:\n"
    var input = readLine()
    var maybeEquation = tryParseEquation(system, input)
    while maybeEquation.isEmpty do
      print(errorMessage)
      input = readLine()
      maybeEquation = tryParseEquation(system, input)
    maybeEquation.get
  }

  def equationsInputter(system: trs.System): Set[Equation] = {
    var equations: Set[Equation] = Set()
    var moreEquations = true
    while moreEquations do {
      equations = equations + equationInputter(system)
      print("Do you want to add another equation? (y/N): ")
      loopForCorrectLowerCaseInput(List("y", "yes", "", "n", "no")) match {
        case "y" | "yes" => moreEquations = true
        case _ => moreEquations = false
      }
    }
    equations
  }


  /** Try to parse an equation string to an equation. Returns [[None]] if not possible. [[Some]]([[Equation]]) otherwise. */
  def tryParseEquation(system: trs.System, string: String): Option[Equation] = {
    val parser = new TRSParser(_ => "")
    parser.parseAll[QuasiRule](parser.rule(parser.equalSign), string) match {
      case parser.Success(quasiRule: QuasiRule, _) =>
        val quasiRuleApp = quasiRule.infix2app(system.signature.asMap)
        val (signature, variableSorts) = QuasiSystem.deriveTypings(system.signature, QuasiSignature(Set.empty), Set(quasiRuleApp))
        val rule = quasiRuleApp.toRule(signature.asMap, variableSorts.map{ case ((_,name),sort) => name -> sort }, true)
        Some(rule.asEquation)
      case parser.Failure(msg, _) => println(msg) ; None
      case parser.Error(msg, _) => println(msg) ; None
    }
  }
}
