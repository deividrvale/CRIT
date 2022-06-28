package equiv

import equiv.ri.Equation.Side
import equiv.ri.{Equation, ProofState}
import equiv.ri.tactics.{COMPLETENESS, CONSTRUCTOR, DELETION, DISPROVE, EQ_DELETION, EXPANSION, GENERALIZATION, POSTULATE, SIMPLIFICATION}

import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap


class CLILogic(var pfSt: ProofState) {
  var forceQuit = false
  val autoId = "-1"
  val quitId = "q"

  val inferenceRuleNrNameActions: ListMap[String, (String, () => Unit)] = ListMap (
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

      inferenceRuleNrNameActions.foreach((nr, nameAction) => println(s" $nr: ${nameAction._1}"))

      print("Choose action id: ")
      var input = readLine().trim.toLowerCase
      while !inferenceRuleNrNameActions.contains(input)
      do {
        print("Action id not recognized. Enter a valid action id: ")
        input = readLine().toLowerCase
      }
      inferenceRuleNrNameActions(input)._2()
    }
    println(s"Rewriting Induction terminated. Reason: " +
        s"${if forceQuit then "force quit"
        else if pfSt.isFinished then "proofstate is in terminal state"
        else if pfSt.isFalse then "proofstate is disproven"
        else "unknown"}.")
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

  /** Prompt the user to choose an equation from the current proofstate.
   * @return `Some(Equation)` if an equation was selected or `None` if the 'auto' option was selected. */
  def chooseEquation(): Option[Equation] = {
    val eqsAndIndices = pfSt.equations.zipWithIndex.map((eq, id) => (id.toString, eq)).toList
    val eqsAndIndicesMap = eqsAndIndices.toMap
    println(s"$autoId: Auto-choose")
    eqsAndIndices.foreach((nr, eq) => println(s" $nr: ${eq.toPrintString()}"))
    println(s"$quitId: Return")
    print("Please choose an equation: ")
    var input = readLine().trim.toLowerCase
    while
      !(eqsAndIndicesMap.contains(input) || input == autoId || input == quitId)
    do {
      print("Equation id not recognized. Enter a valid id: ")
      input = readLine().trim.toLowerCase
    }
    println()
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
    val leftValues = List("0", "l", "left", "le", "lef")
    val rightValues = List("1", "r", "right", "ri", "rig", "righ")
    val autoValues = List("2", "a", "au", "aut", "auto")
    assert(leftValues.intersect(rightValues).isEmpty)
    print("Choose side (l/r/auto): ")
    var input = readLine().trim.toLowerCase
    while
      !(leftValues.contains(input) || rightValues.contains(input) || autoValues.contains(input) || input == quitId)
    do {
      print("Side not recognized. Enter a valid side (l/r/auto): ")
      input = readLine().trim.toLowerCase
    }
    println()
    if input == quitId then None
    else if leftValues.contains(input) then Some(Side.Left)
    else if rightValues.contains(input) then Some(Side.Right)
    else { println("Auto not implemented yet."); None }
  }
}
