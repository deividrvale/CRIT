package equiv

import equiv.ri.{Equation, ProofState}
import equiv.ri.Equation.Side
import equiv.ri.inference_rules.{COMPLETENESS, CONSTRUCTOR, DELETION, DISPROVE, EQ_DELETION, EXPANSION, GENERALIZATION, POSTULATE, SIMPLIFICATION}
import equiv.trs.Rule

import scala.io.StdIn.readLine

object InputHandler {
  val leftOption: String = "l"
  val rightOption: String = "r"
  val inferenceRules: List[String] = List(COMPLETENESS.name, CONSTRUCTOR.name, DELETION.name, DISPROVE.name, EQ_DELETION.name, EXPANSION.name, GENERALIZATION.name, POSTULATE.name, SIMPLIFICATION.name)

  def main(pfSt: ProofState): Unit = {
    while true do
      val x = inferenceRuleSelector(inferenceRules)
      println(s"Selected: $x.\nThank you.\n\n")
      val y = equationSelector(pfSt.equations.toList)
      println(s"Selected: $y.\nThank you.\n\n")
  }

  def loopForCorrectInput(correctInput: List[String]): String = {
    var input = readLine()
    while !correctInput.contains(input) do
      print("Incorrect input, try again: ")
      input = readLine()
    input
  }

  def withIndex[T](input: List[T]): Map[String, T] = input.zipWithIndex.map((data, i) => (i.toString, data)).toMap

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

  def sideSelector(equation: Equation): Side = {
    println("Choose a side:")
    println(s"$leftOption: ${equation.left.toPrintString()}")
    println(s"$rightOption: ${equation.right.toPrintString()}")
    loopForCorrectInput(List(leftOption, rightOption)) match {
      case this.leftOption => Side.Left
      case this.rightOption => Side.Right
    }
  }


}
