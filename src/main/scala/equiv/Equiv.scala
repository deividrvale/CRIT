package equiv

import equiv.ri.{CALCULATION, Equation, ProofState}
import equiv.trs.QueryEquivalence
import equiv.trs.parsing.QuasiQueryEquivalence
import equiv.utils.Z3

object Equiv {
  def main(args: Array[String]): Unit = {
    parse("wouter") match {
      case Some((system, pfSt)) =>
        InputHandler.main(system, pfSt)
      case _ => println("Failed to parse")
    }
  }

  def parse(fileName: String): Option[(trs.System, ProofState)] = {
    TRSParserTest.parseTRS(s"examples/$fileName.ctrs").foreach(
      system =>
        val equations: Set[Equation] = system.query match {
          case Some(QueryEquivalence(equation)) => Set(equation)
          case _ => Set()
        }
        val pfSt: ProofState = ProofState(equations, system.rules)
        return Some((system, pfSt))
    )
    None
  }

}
