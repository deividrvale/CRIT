package equiv

import equiv.ri.inference_rules.{CONSTRUCTOR, DELETION, DISPROVE, EQ_DELETION, EXPANSION, SIMPLIFICATION}
import equiv.ri.{Equation, ProofState}
import tests.SampleObjects.*
import equiv.trs.Term.{App, Position}
import equiv.utils.TermUtils
import tests.SampleObjects.InferenceRuleEquations.*

object Test {
  def main(args: Array[String]): Unit = {
    val equations = Set(constructorEquation, deletionEquation1, deletionEquation2, disproveEquation1, disproveEquation2, disproveEquation3, disproveEquation4)
    val p1 = ProofState(Set(expansionEquation) ++ equations, Set(rule1, rule2))
    println("DELETION: " + DELETION.getDELETIONEquations(p1).map(_.toPrintString()))
    println("CONSTRUCTOR: " + CONSTRUCTOR.getCONSTRUCTOREquations(p1))
    println("DISPROVE: " + DISPROVE.getDISPROVEEquations(p1))
    println("EXPANSION: " + EXPANSION.getEXPANSIONEquations(p1))
    println("SIMPLIFICATION: " + SIMPLIFICATION.getSIMPLIFICATIONEquations(p1))
    println("EQ-DELETION: " + EQ_DELETION.getEQ_DELETIONEquations(p1))
  }
}
