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

    val pfSt = ProofState(Set(equation), sumRecRules, true)

    doRI(pfSt)
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
        currentPfSt.tryEqDeletion().getOrElse(
          currentPfSt.trySimplification().getOrElse( 
            currentPfSt.tryExpansion().getOrElse( 
              { existApplicableTactics = false; currentPfSt } 
            )
          )
        )
       )
      println(currentPfSt.toPrintString())

    println(s"Done after $i iteration(s). Finished: ${currentPfSt.isFinished}.")
  }

}
