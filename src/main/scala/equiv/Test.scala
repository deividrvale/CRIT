package equiv

import equiv.ri.inference_rules.{CONSTRUCTOR, DELETION, DISPROVE, EXPANSION}
import equiv.ri.{Equation, ProofState, Simplify}
import equiv.trs.Temp.*
import equiv.trs.Term.App
import equiv.utils.TermUtils
import equiv.trs.Temp.InferenceRuleEquations.*

object Test {
  def main(args: Array[String]): Unit = {
    val equations = Set(constructorEquation, deletionEquation1, deletionEquation2, disproveEquation1, disproveEquation2, disproveEquation3, disproveEquation4)
    val p1 = ProofState(Set(expansionEquation) ++ equations, Set(rule1, rule2))
    println("DELETION: " + DELETION.getDELETIONEquations(p1).map(_.toPrintString()))
    println("CONSTRUCTOR: " + CONSTRUCTOR.getCONSTRUCTOREquations(p1))
    println("DISPROVE: " + DISPROVE.getDISPROVEEquations(p1))
    println("EXPANSION: " + EXPANSION.getEXPANSIONEquations(p1))

  }
}
