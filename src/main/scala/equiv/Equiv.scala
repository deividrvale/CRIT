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
    import Temp.SumUp.{sumRecRules, equation}

    val eq1: Equation = Equation(termFx, termReturnX, Set(consXEqZero))
    val eq2: Equation = Equation(termFx, termReturnZero, Set(consXLEZero))
    val eq3: Equation = Equation(termFx, termReturnZero, Set())
    val eq4: Equation = Equation(termReturnZero, termFx, Set(consXLEZero, consXEqZero))
    val delEq1: Equation = Equation(termFy, termFy, Set())
    val delEq2: Equation = Equation(termFy, termGy, Set(consVarIntInt("y", ">", 1), consVarIntInt("y", "<", 1)))
    val newPfSt: ProofState = ProofState(Set(eq1, eq2, eq3, eq4, delEq1, delEq2), Set(rho1, rho2), Set(), true)

    val pfSt = ProofState(Set(equation), sumRecRules, Set(), true)

    equiv.CLILogic(pfSt).RI()
  }

  def parse(fileName: String): Unit = {
    TRSParserTest.parseTRS(s"examples/$fileName.ctrs").foreach(
      system =>
        val equations: Set[Equation] = Set() // TODO get starting equation
        val pfSt: ProofState = ProofState(equations, system.rules, Set(), true)
    )
  }


}
