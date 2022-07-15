package equiv

import equiv.ri.inference_rules.{CONSTRUCTOR, DELETION}
import equiv.ri.{ProofState, Simplify}
import equiv.trs.Temp.*
import equiv.trs.Term.App
import equiv.utils.TermUtils
import equiv.trs.Temp.InferenceRuleEquations.*

object Test {
  def main(args: Array[String]): Unit = {
    val p1 = ProofState(Set(constructorEquation, deletionEquation1, deletionEquation2), Set(rule1, rule2))
    println(DELETION.getDELETIONEquations(p1))
    println(CONSTRUCTOR.getCONSTRUCTOREquations(p1))
  }
}
