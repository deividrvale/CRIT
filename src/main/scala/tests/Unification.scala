package tests

import equiv.trs.Term
import equiv.trs.Term.Substitution
import equiv.utils.TermUtils

object Unification {
  def main(): Unit = {
    import tests.SampleObjects.InferenceRuleEquations.u
    import tests.SampleObjects.Sums._
    import tests.SampleObjects.Values._
    testResult(x, x, Some(Map()))
    testResult(one, one, Some(Map()))
    testResult(one, two, None)
    testResult(Term.App(u, List(one, x, y)), Term.App(u, List(z, x, z)), Some(Map((z, one), (y, one))))
    testResult(Term.App(u, List(one, x, two)), Term.App(u, List(z, z, z)), None)
    testResult(Term.App(u, List(x, x, x)), Term.App(u, List(z, z, z)))
    testResult(Term.App(u, List(one, one, one)), Term.App(u, List(z, z, z)), Some(Map((z, one))))
  }

  /** Test if the unification map of two terms is commutatively the same (modulo variable-to-variable maps) and equal to the expected result (if given). */
  def testResult(term1: Term, term2: Term, expectedResult: Option[Substitution] = null): Unit = {
    val t1t2 = term1.unifiableWith(term2)
    val t2t1 = term2.unifiableWith(term1)
    t1t2 match {
      case Some(s) =>
        TermUtils.maybeReverseMap(s) match {
          case Some(reverseSub) =>
            assert(t1t2 == t2t1 || t2t1.contains(reverseSub))
            if expectedResult != null then assert(expectedResult.contains(reverseSub))
          case None =>
            assert(t1t2 == t2t1)
            if expectedResult != null then assert(t1t2 == expectedResult)
        }
      case None =>
        assert(t1t2 == t2t1)
        assert(t1t2 == expectedResult)
    }
    println(s"Assertion success: ${term1.toPrintString()} unifiable with ${term2.toPrintString()}? Result: $expectedResult")
  }
}
