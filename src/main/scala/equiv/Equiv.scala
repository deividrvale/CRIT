package equiv

import equiv.ri.{CALCULATION, Equation, ProofState}
import equiv.trs.QueryEquivalence
import equiv.trs.parsing.QuasiQueryEquivalence
import equiv.utils.Z3

object Equiv {
  def main(args: Array[String]): Unit = {
//    sample()
    parse("wouter") match {
      case Some(pfSt) =>
        debug(pfSt)
        InputHandler.main(pfSt)
      case _ => println("Failed to parse")
    }
  }

  def debug(pfSt: ProofState): Unit = {
//    println(CALCULATION.getEquationSubtermVarReplacementPositionsAux(pfSt.equations.head.left))
//    println(CALCULATION.getEquationSubtermVarReplacementPositionsAux(pfSt.equations.head.right))
//    println(pfSt.equations.head.constraints.head.getEqualityVars)
    val leftRule = pfSt.rules.head.left
    val eq = pfSt.equations.head.left
    println(s"${eq.toPrintString()} unifiable with:\n")
    println(pfSt.rules.map(r => s"${r.left.toPrintString()} ${eq.unifiableWith(r.left)}").mkString("","\n",""))
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
