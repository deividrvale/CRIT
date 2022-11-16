package tests

import equiv.trs.Term

object Unification {
  def main(args: Array[String]): Unit = {
    import tests.SampleObjects.InferenceRuleEquations.u
    import tests.SampleObjects.Sums._
    import tests.SampleObjects.Values._
    val t1 = Term.App(u, List(one, x, y))
    val t2 = Term.App(u, List(z, z, z))
    println(t1.unifiableWith(t2))
  }
}
