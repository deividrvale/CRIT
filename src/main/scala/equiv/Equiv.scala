package equiv

import equiv.ri.{Equation, ProofState}
import equiv.trs.Temp.*
import equiv.utils.Z3

import java.awt.Frame // usable for GUI

object Equiv {
  def main(args: Array[String]): Unit = {    
    sample()
    // parseAndDoRI("decompose")
  }
  
  def sample(): Unit = {
    import equiv.trs.Temp.Sums._

    val rules = sumUpRules ++ sumRecRules

    val pfSt = ProofState(Set(sumUpRecEq), rules)

    InputHandler.main(pfSt)
//    equiv.CLILogic(pfSt).RI()
  }

  def parse(fileName: String): Unit = {
    TRSParserTest.parseTRS(s"examples/$fileName.ctrs").foreach(
      system =>
        val equations: Set[Equation] = Set() // TODO get starting equation
        val pfSt: ProofState = ProofState(equations, system.rules)
    )
  }


}
