package equiv

import equiv.ri.{Equation, ProofState}
import equiv.trs.QueryEquivalence
import equiv.trs.Temp.*
import equiv.trs.parsing.QuasiQueryEquivalence
import equiv.utils.Z3

import java.awt.Frame // usable for GUI

object Equiv {
  def main(args: Array[String]): Unit = {    
//    sample()
    parse("sum") match {
      case Some(pfSt) => InputHandler.main(pfSt)
      case _ => println("Failed to parse")
    }
  }
  
  def sample(): Unit = {
    import equiv.trs.Temp.Sums._
    import equiv.trs.Temp.InferenceRuleEquations._

    val rules = sumUpRules ++ sumRecRules

    val pfSt = ProofState(Set(
      expEq2, eqDelEq, eqDelEq2, eqDelEq3, eqDelEq4, eqDelEq5, eqDelEq6
    ),
//      Set(fRule, fRule2),
      Set(),
//      rules,
    )

    InputHandler.main(pfSt)
  }

  def parse(fileName: String): Option[ProofState] = {
    TRSParserTest.parseTRS(s"examples/$fileName.ctrs").foreach(
      system =>
        val equations: Set[Equation] = system.query match {
          case Some(QueryEquivalence(equation)) => Set(equation)
          case _ => Set()
        }
        val pfSt: ProofState = ProofState(equations, system.rules)
//        println(system)
        return Some(pfSt)
    )
    None
  }


}
