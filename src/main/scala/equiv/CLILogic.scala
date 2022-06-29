package equiv

import equiv.ri.Equation.Side
import equiv.ri.{Equation, ProofState}
import equiv.ri.tactics.{COMPLETENESS, CONSTRUCTOR, DELETION, DISPROVE, EQ_DELETION, EXPANSION, GENERALIZATION, POSTULATE, SIMPLIFICATION}
import equiv.trs.Rule

import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap


class CLILogic(var pfSt: ProofState) {
  val stringAfterCorrectUserInput: String = "\n" * 5

  var forceQuit: Boolean = false
  val autoId: String = "-1"
  val quitId: String = "q"

  val leftValues: List[String] = List("0", "l", "left", "le", "lef", "links")
  val rightValues: List[String] = List("1", "r", "right", "ri", "rig", "righ", "rechts")
  val autoValues: List[String] = List("2", "a", "au", "aut", "auto")
  assert(leftValues.intersect(rightValues).isEmpty)
  val noValues: List[String] = List("0", "n", "no", "nee")
  val yesValues: List[String] = List("1", "y", "ye", "yes", "ja")
  assert(noValues.intersect(yesValues).isEmpty)

  val actions: ListMap[String, (String, () => Unit)] = ListMap (
    autoId -> ("AUTO", () => println("Not implemented yet.")),
    "0" -> ("Simplify with calc", () => simplify_calc()),
    "1" -> ("DELETION", () => deletion()),
    "2" -> ("CONSTRUCTOR", () => constructor()),
    "3" -> ("EQ-DELETION", () => eq_deletion()),
    "4" -> ("SIMPLIFICATION", () => simplification()),
    "5" -> ("EXPANSION", () => expansion()),
    "6" -> ("POSTULATE", () => postulate()),
    "7" -> ("GENERALIZE", () => generalize()),
    "8" -> ("COMPLETENESS", () => completeness()),
    "9" -> ("DISPROVE", () => disprove()),
    quitId -> ("QUIT", () => forceQuit = true),
  )

  def RI(): Unit = {
    println(s"Starting Rewriting Induction with proofstate")

    while
      !pfSt.isFinished && !forceQuit
    do {
      println(s"\n${pfSt.toPrintString()}\n")

      actions.foreach((nr, nameAction) => println(s" $nr: ${nameAction._1}"))

      print(s"${Console.UNDERLINED}Choose action id${Console.RESET}: ")
      var input = readLine().trim.toLowerCase
      while !(actions.contains(input))
      do {
        print("Action id not recognized. Enter a valid action id: ")
        input = readLine().toLowerCase
      }
      print(stringAfterCorrectUserInput)
      println(actions(input)._1)
      actions(input)._2()
    }
    println(s"Rewriting Induction terminated. Reason: " +
      s"${if forceQuit then "force quit"
      else if pfSt.isFinished then "proofstate is in terminal state"
      else if pfSt.isFalse then "proofstate is disproven"
      else "unknown"}.")
  }

  /** Prompt the user to choose an equation from the current proofstate.
   * @return `Some(Equation)` if an equation was selected or `None` if the 'auto' option was selected. */
  def chooseEquation(): Option[Equation] = {
    val eqsAndIndices = pfSt.equations.toSeq.zipWithIndex.map((eq, id) => (id.toString, eq))
    val eqsAndIndicesMap = eqsAndIndices.toMap
    println(s"$autoId: Auto-choose")
    eqsAndIndices.foreach((nr, eq) => println(s" $nr: ${eq.toPrintString()}"))
    println(s" $quitId: Return")
    print(s"${Console.UNDERLINED}Please choose an equation${Console.RESET}: ")
    var input = readLine().trim.toLowerCase
    while
      !(eqsAndIndicesMap.contains(input) || input == autoId || input == quitId)
    do {
      print("Equation id not recognized. Enter a valid id: ")
      input = readLine().trim.toLowerCase
    }
    print(stringAfterCorrectUserInput)
    println(s"Equation ${eqsAndIndicesMap(input)}.")
    if input == autoId then
      println("Auto not supported yet")
      None // TODO
    else if input == quitId then
      None
    else
      Some(eqsAndIndicesMap(input))
  }

  /** Prompt the user to choose a side of an equation.
   * @return `Some(Side.Left)` or `Some(Side.Right)` or None if the 'auto' option was selected */
  def chooseSide(): Option[Side] = {
    println(s" $quitId: Return")
    print(s"${Console.UNDERLINED}Choose side (l/r/auto)${Console.RESET}: ")
    var input = readLine().trim.toLowerCase
    while
      !(leftValues.contains(input) || rightValues.contains(input) || autoValues.contains(input) || input == quitId)
    do {
      print("Side not recognized. Enter a valid side (l/r/auto): ")
      input = readLine().trim.toLowerCase
    }
    print(stringAfterCorrectUserInput)
    if input == quitId then None
    else if leftValues.contains(input) then Some(Side.Left)
    else if rightValues.contains(input) then Some(Side.Right)
    else { println("Auto not implemented yet."); None }
  }

  /** Prompt the user to choose whether to add the given rule to the hypotheses set or not.
   * @return `Some(true)` if the user chose ''yes'', `Some(false)` if the user chose ''no'', `None` if the user chose ''quit'' or ''auto'' */
  def chooseAddRule(rule: Rule): Option[Boolean] = {
    println(s"Rule generated: ${rule.toPrintString()}")
    println(s" $quitId: Return")
    print(s"${Console.UNDERLINED}Do you want to add this rule? (y/n/a)${Console.RESET}: ")
    var input = readLine().trim.toLowerCase
    while
      !(yesValues.contains(input) || noValues.contains(input) || autoValues.contains(input) || input == quitId)
    do
      print("Answer not recognized. Please enter a valid answer (y/n/a): ")
      input = readLine().trim.toLowerCase
    print(stringAfterCorrectUserInput)
    if input == quitId then None
    else if noValues.contains(input) then Some(false)
    else if yesValues.contains(input) then Some(true)
    else { println("Auto not implemented yet.") ; None }
  }

  def simplify_calc(): Unit = {
  }

  def deletion(): Unit = {
    chooseEquation()
      .foreach( eq => DELETION.deletable(eq)
        .map( _ => pfSt = pfSt.removeEquation(eq) )
        .getOrElse( println("DELETION failed") )
      )
  }

  def constructor(): Unit = {
    chooseEquation()
      .foreach( eq => CONSTRUCTOR.tryConstructorOnEquation(eq)
        .map( eqs => pfSt = pfSt.removeEquation(eq).addEquations(eqs) )
        .getOrElse(println("CONSTRUCTOR failed") )
      )
  }

  def eq_deletion(): Unit = {
    chooseEquation()
      .foreach( eq => EQ_DELETION.tryEqDeletionOnEquation(eq, pfSt)
        .map( newEq => pfSt = pfSt.replaceEquationWith(eq, newEq) )
        .getOrElse( println("EQ-DELETION failed") )
      )
  }

  def simplification(): Unit = {
    chooseEquation()
      .map( eq => chooseSide()
        .map( side => SIMPLIFICATION.trySimplificationOnEquationSide(eq, side, pfSt.rules)
            .map( newEq => pfSt = pfSt.replaceEquationWith(eq, newEq) )
            .getOrElse( println("SIMPLIFICATION failed") )
        )
      )
  }

  def expansion(): Unit = {
    chooseEquation()
      .map( eq => chooseSide()
        .map( side => EXPANSION.tryExpansionOnEquationSide(eq, side, pfSt.rules)
          .map( (eqs, maybeRule) => {
            pfSt = pfSt.removeEquation(eq).addEquations(eqs)
            maybeRule.map( rule =>
              chooseAddRule(rule)
                .map( addRule => if addRule then pfSt = pfSt.addRule(rule) )
            ).getOrElse( println("No suitable rule found (not terminating).") )
          })
        )
      )
  }

  def postulate(): Unit = {
  }

  def generalize(): Unit = {
  }

  def completeness(): Unit = {
    COMPLETENESS.tryCompleteness(pfSt)
      .map( newPfst => pfSt = newPfst )
      .getOrElse( println("COMPLETENESS failed") )
  }

  def disprove(): Unit = {
    chooseEquation()
      .foreach( eq => DISPROVE.tryDisproveOnEquation(eq, pfSt)
        .map( _ => pfSt.isFalse = true )
        .getOrElse( println("DISPROVE failed") ))
  }

}
