package equiv

import equiv.ri.{CALCULATION_SIMP, CALCULATION_VARREPL, Equation, ProofState}
import equiv.ri.Equation.Side
import equiv.ri.inference_rules.{COMPLETENESS, CONSTRUCTOR, DELETION, DISPROVE, EQ_DELETION, EXPANSION, GENERALIZATION, POSTULATE, SIMPLIFICATION}
import equiv.trs.Term.Position
import equiv.trs.parsing.{QuasiQuery, QuasiQueryEquivalence, QuasiRule, QuasiSignature, QuasiTerm, TRSParser}
import equiv.trs.{FunctionSymbol, Rule, Signature, Sort, Term, Typing}
import equiv.utils.OptionExtension.printRedOnNone
import equiv.utils.{PrintUtils, TermUtils, Z3}

import java.io.StringReader
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
        DISPROVE.tryDISPROVE(pfSt).map(_ => {
          endMessage = PrintUtils.successColourString("DISPROVE. Rewriting Induction terminated.") ; pfSt.removeAllEquations()
        })
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
        val signature = system.signature.asMap
        val quasiRuleNoInfix = QuasiRule(quasiRule.left.infix2app(signature), quasiRule.right.infix2app(signature), quasiRule.constraint.map(_.infix2app(signature)))
        deriveVariableSorts(Set(quasiRuleNoInfix), QuasiSignature(system.signature.functions.map(Left(_)))) match {
          case Left(error) => println(PrintUtils.failureColourString(error)) ; None
          case Right(variableSorts) => Some(quasiRuleNoInfix.toEquation(signature, variableSorts))
        }
      case parser.Failure(msg, _) => println(msg) ; None
      case parser.Error(msg, _) => println(msg) ; None
    }
  }

  /** All code below pretty much copied from [[QuasiSignature.deriveTypings]].
   * Author's note: this function probably belongs in a different class, but I am too lazy to move it.
   * @author Wouter
   * @return [[Left]]([[String]]) if there was a parsing error. The string contains the error message. Make sure to print this.
   * If there was no error, return [[Right]]([[Map]]), where [[Map]] a map that maps variable names to their sort. */
  def deriveVariableSorts(rules: Set[QuasiRule], signatureOriginal: QuasiSignature): Either[String, Map[String, Sort]] = {
    // a function argument (Some(nr)) or output (None)
    type Port = (Any, Option[Int])

    val allRules = rules

    val usedSymbols = allRules.flatMap(_.functionSymbols)
    val intSymbols = usedSymbols.filter(_._2 == 0).filter(_._1.toIntOption.nonEmpty).map(_._1)
    val intSignature = QuasiSignature(intSymbols.map { i => Left(FunctionSymbol(i, Typing(List.empty, Sort.Int), isTheory = true, isValue = true)) })
    val signature = signatureOriginal.union(intSignature)

//    val signatureSymbols = signature.asMap.map(Right(_))
//    val signatureSymbolsTyped = signature.asMap

    val signatureSymbols = signature.asMap
    val signatureSymbolsTyped = signature.leftAsMap

    // function symbols with arities
    var symbol2arity = signature.left.map { symbol => symbol.name -> (symbol.typing.input.length, symbol.isTheory, symbol.isValue, symbol.typing.isVariadic) }.toMap
    usedSymbols.filter { s => signatureSymbols.contains(s._1) || s._2 > 0 }.foreach { case (symbol, arity) =>
      if (symbol2arity.contains(symbol)) {
        val (theArity, theory, isValue, variadic) = symbol2arity(symbol)
        if (theArity != arity && !variadic) return Left(s"The symbol $symbol occurs with varying arities.")
      } else {
        if (!symbol2arity.contains(symbol)) symbol2arity = symbol2arity.updated(symbol, (arity, false, false, false))
      }
    }

    // symbol arguments as ports
    def portOfSymbol(symbol: String, arg: Option[Int]): Port = {
      arg match {
        case Some(argNr) => (symbol, Some(Math.min(argNr, symbol2arity(symbol)._1 - 1)))
        case None => (symbol, None)
      }
    }

    // variables as ports
    def portOfVariable(rule: QuasiRule, name: String): ((QuasiRule, String), Option[Int]) = ((rule, name), None)

    val variables: Set[((QuasiRule, String), Option[Int])] = allRules.flatMap { rule =>
      rule.functionSymbols.filter { symbol => symbol._2 == 0 && !symbol2arity.contains(symbol._1) }.map { symbol => portOfVariable(rule, symbol._1) }
    }

    // checks if the given argument is polymorphic
    def isSortAny(symbol: String, arg: Option[Int]): Boolean = {
      signatureSymbolsTyped.get(symbol).exists { functionSymbol =>
        val typing = functionSymbol.typing
        portOfSymbol(symbol, arg) match {
          case (_, Some(nr)) => typing.input(nr) == Sort.Any
          case (_, None) => typing.output == Sort.Any
        }
      }
    }

    // ports (excluding sort Any)
    var port2class: Map[Port, Int] = (symbol2arity.toList.flatMap { case (symbol, (arity, _, _, _)) =>
      ((symbol, None) :: (0 until arity).toList.map { i => (symbol, Some(i)) }).filterNot(isSortAny)
    } ++ variables).zipWithIndex.toMap

    def setPortsEqual(a: Port, b: Port): Unit = {
      val aclass = port2class(a)
      val bclass = port2class(b)
      if (aclass != bclass) port2class = port2class.view.mapValues { v => if (v == aclass) bclass else v }.toMap
    }

    def deriveEquality(rule: QuasiRule, term: QuasiTerm): Port = {
      term match {
        case QuasiTerm.App(symbol, args) =>
          val argPorts: List[Port] = args.indices.map { i => deriveEquality(rule, args(i)) }.toList
          // not Any
          args.indices.foreach { i =>
            if (!isSortAny(symbol, Some(i))) setPortsEqual(argPorts(i), (symbol, Some(i)))
          }
          // Any
          val argPortsAny: List[Port] = args.indices.filter { i => isSortAny(symbol, Some(i)) }.map(argPorts).toList
          if (argPortsAny.nonEmpty) {
            argPortsAny.sliding(2, 1).foreach {
              case List(a, b) => setPortsEqual(a, b)
              case _ =>
            }
          }

          if (symbol2arity.contains(symbol)) {
            if (isSortAny(symbol, None)) argPortsAny.head else (symbol, None)
          } else portOfVariable(rule, symbol)
      }
    }

    allRules.foreach { rule =>
      setPortsEqual(deriveEquality(rule, rule.left), deriveEquality(rule, rule.right))
      rule.constraint.foreach(deriveEquality(rule, _))
    }

    // derive types
    var class2sort: Map[Int, Sort] = Map.empty

    /** If there is an error, return [[Some]]([[String]]) with error message. Otherwise return [[None]] */
    def setClass2Sort(clazz: Int, sort: Sort): Option[String] = {
      if (class2sort.get(clazz).exists(_ != sort)) {
        return Some(s"There is a typing conflict ${class2sort(clazz)} != $sort.")
      }
      class2sort = class2sort.updated(clazz, sort)
      None
    }

    signature.left.foreach { symbol =>
      if (!isSortAny(symbol.name, None)) setClass2Sort(port2class((symbol.name, None)), symbol.typing.output).map(errorMessage => return Left(errorMessage))
      symbol.typing.input.indices.foreach { i =>
        if (!isSortAny(symbol.name, Some(i))) setClass2Sort(port2class((symbol.name, Some(i))), symbol.typing.input(i)).map(errorMessage => return Left(errorMessage))
      }
    }

    // set missing types to "result"
    val default = Sort("result", false)
    symbol2arity.foreach { case (symbol, (arity, theory, isValue, variadic)) =>
      (0 until arity).foreach { i =>
        if (isSortAny(symbol, Some(i))) Sort.Any
        else class2sort.get(port2class((symbol, Some(i)))) match {
          case None => class2sort = class2sort.updated(port2class((symbol, Some(i))), default)
          case _ =>
        }
      }

      if (isSortAny(symbol, None)) Sort.Any else
        class2sort.get(port2class((symbol, None))) match {
          case None => class2sort = class2sort.updated(port2class((symbol, None)), default)
          case _ =>
        }
    }

    val maybeSignature: Either[String, Signature] = {
      // typing for each symbol
      Right(Signature(symbol2arity.map { case (symbol, (arity, theory, isValue, variadic)) =>
        val inputSorts = (0 until arity).toList.map { i =>
          if (isSortAny(symbol, Some(i))) Sort.Any
          else class2sort.get(port2class((symbol, Some(i)))) match {
            case Some(sort) => sort
            case None => return Left(s"Failed to derive the type of $symbol argument $i.")
          }
        }

        val outputSort = if (isSortAny(symbol, None)) Sort.Any else
          class2sort.get(port2class((symbol, None))) match {
            case Some(sort) => sort
            case None => return Left(s"Failed to derive the output sort of $symbol.")
          }

        FunctionSymbol(symbol, Typing(inputSorts, outputSort, isVariadic = variadic), theory, isValue, signature.left.find(_.name == symbol).flatMap(_.infix))
      }.toSet))
    }

    maybeSignature match {
      case Left(error) => Left(error)
      case Right(s) =>
        val signatureDifference = s.functions -- signature.getFunctionSymbols
        if (signatureDifference.nonEmpty) then
          Left(s"Unrecognized function symbols: ${signatureDifference.mkString(", ") }")
        else
        Right(
        variables.map { case port@((_, name), _) =>
          val x = port2class(port)
          val y = if (!class2sort.contains(x)) Sort.Any else class2sort(x)
          name -> y }.toMap
      )
    }
  }


}
