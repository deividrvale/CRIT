package tests

import equiv.InputHandler
import equiv.ri.ProofState
import tests.SampleObjects.InferenceRuleEquations.constructorEquation

object EquivTest {
  def main(): Unit = {
    import tests.SampleObjects.Sums._

    val rules = sumUpRules ++ sumRecRules

    val pfSt = ProofState(Set(
      //      expEq2, eqDelEq, eqDelEq2, eqDelEq3, eqDelEq4, eqDelEq5, eqDelEq6
      constructorEquation
    ),
      //      Set(fRule, fRule2),
      Set(),
      //      rules,
    )

//    InputHandler.main(pfSt)
  }

}
