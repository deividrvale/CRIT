package equiv

import equiv.ri.{Equation, ProofState}
import equiv.trs.Temp.*
import equiv.utils.Z3

import java.awt.Frame // usable for GUI

object Equiv {
  def main(args: Array[String]): Unit = {    
    sample()
  }
  
  def sample(): Unit = {
    import equiv.trs.Temp.Sums._
    import equiv.trs.Temp.InferenceRuleEquations._

    val rules = sumUpRules ++ sumRecRules

    val pfSt = ProofState(Set(
      expEq2
    ),
      Set(fRule)
//      rules
    )

    InputHandler.main(pfSt)
  }

  def parse(fileName: String): Unit = {
    TRSParserTest.parseTRS(s"examples/$fileName.ctrs").foreach(
      system =>
        val equations: Set[Equation] = Set() // TODO get starting equation
        val pfSt: ProofState = ProofState(equations, system.rules)
    )
  }


}
