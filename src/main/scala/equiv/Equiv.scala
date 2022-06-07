package equiv

import equiv.ri.Rewrite.{rewriteAtPos, *}
import equiv.trs.*
import equiv.trs.Temp.*
import equiv.trs.Term.*
import equiv.trs.Core.boolTrue

object Equiv {
  def main(args: Array[String]): Unit = {
    println("Hello")
    val x2 = substituteAll(App(FunctionSymbol("g", Typing(List(Sort.Int, Sort.Int), Sort.Int)), List(termFxMinOne, termFxMinOne)), Var("x", Sort.Int), Var("y", Sort.Int))
    val x3 = getTermVars(x2)
    val x1 = rewriteAtPos(ConstrainedTerm(termFxMinOne, consXGEZero),List(0,1),Rule(termFxMinOne, x2, Some(Constraint(boolTrue))))

    val y = isRuleApplicableAtRoot(ConstrainedTerm(substituteAll(termFx, Var("x", Sort.Int), Var("y", Sort.Int)), Constraint(Core.boolTrue)), Rule(termFx, termFxMinOne, None))
    println(y)
  }

}
