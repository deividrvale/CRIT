package equiv

import equiv.ri.Rewrite.*
import equiv.trs.{Constraint, Sort, Temp, Term, ConstrainedTerm, Rule}
import equiv.trs.Temp.{consXGEZero, termFxMinOne, valOne, valZero}
import equiv.trs.Term.Var
import equiv.trs.Core.boolTrue

object Equiv {
  def main(args: Array[String]): Unit = {
    println("Hello")
    val x1 = rewriteAtPos(ConstrainedTerm(termFxMinOne, consXGEZero),List(0,0),Rule(termFxMinOne, termFxMinOne, Some(Constraint(boolTrue))))
    val x2 = substituteAll(termFxMinOne, termFxMinOne, Var("x", Sort.Int))
    println(x2)
  }
}
