package equiv

import equiv.ri.{Equation, ProofState}
import equiv.trs.*
import equiv.trs.Temp.*
import equiv.trs.Term.Var
import equiv.trs.Term
import equiv.utils.TermUtils.*
import equiv.utils.Z3

import scala.io.StdIn.readLine
import equiv.ri.tactics.{DELETION, EQ_DELETION, CONSTRUCTOR, POSTULATE, GENERALIZATION, SIMPLIFICATION}
import equiv.ri.tactics.EXPANSION
import equiv.ri.tactics.COMPLETENESS
import equiv.ri.tactics.DISPROVE

object Equiv {
  def main(args: Array[String]): Unit = {    
    sample()
    // parseAndDoRI("decompose")
  }
  
  def sample(): Unit = {
    import Temp.SumRec.{sumRecRules, equation}

    val eq1: Equation = Equation(termFx, termReturnX, Set(consXEqZero))
    val eq2: Equation = Equation(termFx, termReturnZero, Set(consXLEZero))
    val eq3: Equation = Equation(termFx, termReturnZero, Set())
    val delEq1: Equation = Equation(termFy, termFy, Set())
    val delEq2: Equation = Equation(termFy, termGy, Set(consVarIntInt("y", ">", 1), consVarIntInt("y", "<", 1)))
    val newPfSt: ProofState = ProofState(Set(eq1), Set(rho1, rho2), true)

    val pfSt = ProofState(Set(equation), sumRecRules, true)

    doRI(newPfSt)
  }

  def parseAndDoRI(fileName: String): Unit = {
    TRSParserTest.parseTRS(s"examples/$fileName.ctrs").map(
      system =>
        val equations: Set[Equation] = Set() // TODO get starting equation
        val pfSt: ProofState = ProofState(equations, system.rules, true)
        doRI(pfSt)
    )
  }

  /** Map of strings that correspond to certain tactics. The map can be accessed to apply these tactics to certain proofstates. */
  val tacticInputs: Map[String, (String, ProofState => Option[ProofState])] = Map(
    "1" -> ("DELETION", (pfSt => DELETION.tryDeletion(pfSt))),
    "2" -> ("CONSTRUCTOR", (pfSt => CONSTRUCTOR.tryConstructor(pfSt))),
    "3" -> ("EQ-DELETION", (pfSt => EQ_DELETION.tryEqDeletion(pfSt))),
    "4" -> ("SIMPLIFICATION", (pfSt => SIMPLIFICATION.trySimplification(pfSt))),
    "5" -> ("EXPANSION", (pfSt => EXPANSION.tryExpansion(pfSt))),
    "6" -> ("POSTULATE (Not implemented yet)", (pfSt => None)),
    "7" -> ("GENERALIZE (Not implemented yet)", (pfSt => None)),
    "8" -> ("DISPROVE (Not implemented yet)", (pfSt => None)),
    "9" -> ("COMPLETENESS", (pfSt => Some(COMPLETENESS.doCompleteness(pfSt)))),
    "0" -> ("Simplify with calc (Not implemented yet)", (pfSt => None)),
  )

  def doRI(startingPfSt: ProofState, maxIterations: Int = 50): Unit = {
    println(s"\nStarting Rewriting Induction...\n")
    println(startingPfSt.toPrintString())

    var currentPfSt = startingPfSt
    var existApplicableTactics = true
    var i = 0
    while 
      !currentPfSt.isFinished && i < maxIterations && existApplicableTactics
    do
      i += 1
      println("\nChoose which inference rule to apply. (Type the number.)")
      tacticInputs.toSeq.sortBy(_._1).foreach((nr, nameMethod) => println(s"$nr: ${nameMethod._1}"))
      println("Q: Quit")
      println()
      
      // Read user input
      var choice = readLine.trim
      while !validInputs.contains(choice) 
      do {
        println("Please enter a valid number.")
        choice = readLine.trim
      }
      if tacticInputs.contains(choice) then {
        currentPfSt = tacticInputs(choice)._2(currentPfSt).getOrElse{ { println("Rule failed.") ; currentPfSt } }
      } else if choice.toUpperCase == "Q" then {
        return
      }

      println(currentPfSt.toPrintString())

    println(s"Done after $i iteration(s). Finished: ${currentPfSt.isFinished}.")
  }

  def validInputs: Set[String] = tacticInputs.keySet ++ Set("Q")

}
