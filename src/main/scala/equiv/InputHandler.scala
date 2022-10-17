package equiv

import equiv.ri.{Equation, ProofState}
import equiv.ri.Equation.Side
import equiv.ri.inference_rules.{COMPLETENESS, CONSTRUCTOR, DELETION, DISPROVE, EQ_DELETION, EXPANSION, GENERALIZATION, POSTULATE, SIMPLIFICATION}
import equiv.trs.Term.Position
import equiv.trs.{Rule, Term}
import equiv.utils.OptionExtension.printFailureOnNone

import scala.collection.immutable.ListMap
import scala.io.StdIn.readLine

object InputHandler {
  val inferenceRules: List[String] = List(
    COMPLETENESS.name,
    CONSTRUCTOR.name,
    DELETION.name,
    DISPROVE.name,
    EQ_DELETION.name,
    EXPANSION.name,
    GENERALIZATION.name,
    POSTULATE.name,
    SIMPLIFICATION.name)

  def main(initialPfSt: ProofState): Unit = {
    var pfSt = initialPfSt
    while true do
      println(pfSt.toPrintString())
      (inferenceRuleSelector(inferenceRules) match {
        case CONSTRUCTOR.name =>
          CONSTRUCTOR.tryCONSTRUCTOR(pfSt, equationSelector)
        case COMPLETENESS.name =>
          COMPLETENESS.tryCOMPLETENESS(pfSt)
        case DELETION.name =>
          DELETION.tryDELETION(pfSt, equationSelector)
        case DISPROVE.name =>
          DISPROVE.tryDISPROVE(pfSt).map(_ => pfSt)
        case EQ_DELETION.name =>
          EQ_DELETION.tryEQ_DELETION(pfSt, equationSelector, positionsSelector)
        case EXPANSION.name =>
          EXPANSION.tryEXPANSION(pfSt, equationSelector, sideSelector, positionSelector, ruleAcceptor)
        case GENERALIZATION.name =>
          None
        case POSTULATE.name =>
          None
        case SIMPLIFICATION.name =>
          SIMPLIFICATION.trySIMPLIFICATION(pfSt, equationSelector, sideSelector, ruleSelector, positionSelector)
      }).printFailureOnNone("Inference rule").foreach(pfSt = _) // If tactic fails print this. Otherwise, change the pfSt to the new pfSt
  }

  def ruleAcceptor(rule: Rule): Boolean = {
    println("Do you want to add this rule to the hypotheses set? (Y/n)")
    println(rule.toPrintString())
    loopForCorrectInput(List("y", "", "yes", "no", "n")) match {
      case "" | "y" | "yes" => return true
      case "n" | "no" => return false
    }
    false
  }

  def loopForCorrectInput(correctInput: List[String]): String = {
    var input = readLine()
    while !correctInput.contains(input) do
      print("Incorrect input, try again: ")
      input = readLine()
    input
  }

  /** @return A sorted [[Map]] from integers (as strings) to objects of type [[T]] */
  def withIndex[T](input: List[T]): Map[String, T] = ListMap(input.zipWithIndex.map((data, i) => (i.toString, data)).sortBy(_._1):_*)

  /** Prompt the user to choose a value from a list.
   * @param input The list to choose an option from
   * @param itemToString Function that transforms the list items to how they should be displayed to the user
   * @return An item from the given list */
  def selectFromList[T](input: List[T], itemToString: T => String): T = {
    val listWithIndex = withIndex(input)
    listWithIndex.foreach((i, d) => println(s"$i: ${itemToString(d)}"))
    listWithIndex(loopForCorrectInput(listWithIndex.keys.toList))
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

  def positionSelector(positions: List[Position]): Position = {
    println("Choose a subterm:")
    selectFromList(positions, Term.positionToString)
  }

  def positionsSelector(positions: List[Position]): List[Position] = {
    println("Choose positions:")
    List(positionSelector(positions))
  }

  def sideSelector(sides: List[Side]): Side = {
    println("Choose a side:")
    selectFromList(sides, { case Side.Left => "Left"; case Side.Right => "Right" })
  }

  def equationSideSelector(equation: Equation): Side = {
    println("Choose a side:")
    selectFromList(List(Side.Left, Side.Right), side => equation.getSide(side).toPrintString())
  }

}
