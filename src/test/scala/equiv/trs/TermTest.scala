package equiv.trs

import equiv.utils.TermUtils
import equiv.sample.SampleObjects.{f, one, termFx, two, x, y, z, zero}
import equiv.trs.Term.Var
import org.junit.Assert.{assertEquals, assertTrue}

class TermTest {
  @org.junit.Test
  def getEqualityVarsTest(): Unit = {
    def check(expectedResult: List[Var], inputTerm: Term): Unit = {
      val result = inputTerm.getEqualityVars
      assertTrue(expectedResult.size == result.size && expectedResult.forall(result.contains) && result.forall(expectedResult.contains))
    }
    def eqq(t1: Term, t2:Term) = Term.App(TermUtils.getEqualityFunctionSymbol, List(t1, t2))

    check(List(), one)
    check(List(), termFx)
    check(List(x), eqq(x, one))
    check(List(x, y), Term.App(TermUtils.getEqualityFunctionSymbol, List(x, y, one)))
    check(List(z, x), Term.App(TermUtils.getEqualityFunctionSymbol, List(x, Term.App(f, List(y)), z, one)))
  }

  @org.junit.Test
  def getAssignmentsToTermTest(): Unit = {
    def get_eq(l: List[Term]) = Term.App(TermUtils.getEqualityFunctionSymbol, l)

    assertEquals(Set(), get_eq(List(x, one)).getVarsAssignedToTerm(zero))
    assertEquals(Set(), get_eq(List(x, one)).getVarsAssignedToTerm(two))
    assertEquals(Set(), get_eq(List(x, one)).getVarsAssignedToTerm(termFx))
    assertEquals(Set(x), get_eq(List(x, one)).getVarsAssignedToTerm(one))
    assertEquals(Set(x, y), get_eq(List(x, one, y, two)).getVarsAssignedToTerm(one))

    assertEquals(Set(), get_eq(List(termFx, one)).getVarsAssignedToTerm(one))
    
  }
}
