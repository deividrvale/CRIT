package equiv

import equiv.ri.{Equation, ProofState}
import equiv.trs.QueryEquivalence
import equiv.trs.Temp.*
import equiv.trs.parsing.QuasiQueryEquivalence
import equiv.utils.Z3

import java.awt.Frame // usable for GUI

object Equiv {
  def main(args: Array[String]): Unit = {    
    //sample()
    parse("sum")
  }
  
  def sample(): Unit = {
    import equiv.trs.Temp.Sums._
    import equiv.trs.Temp.InferenceRuleEquations._

    val rules = sumUpRules ++ sumRecRules

    val pfSt = ProofState(Set(
      expEq2
    ),
      Set(fRule, fRule2)
//      rules
    )

    InputHandler.main(pfSt)
  }

  def parse(fileName: String): Unit = {
    TRSParserTest.parseTRS(s"examples/$fileName.ctrs").foreach(
      system =>
        val equations: Set[Equation] = system.query match {
          case Some(QueryEquivalence(equation)) => Set(equation)
          case _ => Set()
        }
        val pfSt: ProofState = ProofState(equations, system.rules)
        println(pfSt)
    )
  }


}
