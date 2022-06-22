package equiv

import equiv.ri.{Equation, ProofState}
import equiv.trs.*
import equiv.trs.Temp.*
import equiv.trs.Term.Var
import equiv.trs.Term
import equiv.utils.TermUtils.constraintTrue
import equiv.utils.Z3

import scala.io.StdIn.readLine

object Equiv {
  def main(args: Array[String]): Unit = {    
    sample()
    // parseAndDoRI("decompose")
  }

  def parseAndDoRI(fileName: String): Unit = {
    val system = TRSParserTest.parseTRS(s"examples/$fileName.ctrs")
    system match {
      case Some(s) => {
        val equations: Set[Equation] = Set() // TODO get starting equation
        val pfSt: ProofState = ProofState(equations, s.rules, true)
        doRI(pfSt)
      }
      case None => println("Failed to parse.")
    }
  }

  def doRI(pfSt: ProofState, maxIterations: Int = 10): Unit = {
    println("Starting state:\n" + pfSt.toPrintString())

    var currentPfSt = pfSt

    var i = 0
    while 
      !pfSt.isFinished && i < maxIterations
    do
      i += 1
      // TODO: RI tactics
      currentPfSt = currentPfSt.tryDeletion()
      currentPfSt = currentPfSt.trySimplification()
      currentPfSt = currentPfSt.tryExpansion()
      println(currentPfSt.toPrintString())

    println("Done")
  }

  def sample(): Unit = {
    val eq1: Equation = Equation(termFy, termReturnZero, Set())
    val delEq1: Equation = Equation(termFy, termFy, Set())
    val delEq2: Equation = Equation(termFy, termGy, Set(consVarIntInt("y", ">", 1), consVarIntInt("y", "<", 1)))
    val newPfSt: ProofState = ProofState(Set(eq1), Set(rho1, rho2), true)

    doRI(newPfSt)
  }


}
