package equiv

import equiv.ri.{Equation, ProofState}
import equiv.trs.*
import equiv.trs.Temp.*
import equiv.trs.Term.Var
import equiv.trs.Term
import equiv.utils.TermUtils.*
import equiv.utils.Z3

import scala.io.StdIn.readLine

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

  def doRI(startingPfSt: ProofState, maxIterations: Int = 50): Unit = {
    println(s"Starting Rewriting Induction...")
    println(startingPfSt.toPrintString())

    var currentPfSt = startingPfSt
    var existApplicableTactics = true
    var i = 0
    while 
      !currentPfSt.isFinished && i < maxIterations && existApplicableTactics
    do
      i += 1
      // TODO: RI tactics
      currentPfSt = 
      currentPfSt.tryDeletion().getOrElse( 
      currentPfSt.tryConstructor().getOrElse(
      currentPfSt.tryEqDeletion().getOrElse(
      currentPfSt.trySimplification().getOrElse( 
      currentPfSt.tryExpansion().getOrElse( 
      { existApplicableTactics = false; currentPfSt } 
      )))))
      println(currentPfSt.toPrintString())

    println(s"Done after $i iteration(s). Finished: ${currentPfSt.isFinished}.")
  }

}
