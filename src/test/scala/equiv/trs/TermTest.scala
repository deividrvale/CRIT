package equiv.trs

import equiv.utils.{TermUtils, TheorySymbols}
import equiv.sample.SampleObjects.{customAddInt, f, one, termFx, two, x, y, z, zero}
import equiv.trs.Term.Var
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

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

  @org.junit.Test
  def isCalculationContainingVariablesTest(): Unit = {
    for (symbol <- List(TheorySymbols.add, TheorySymbols.mul, TheorySymbols.div, TheorySymbols.min)) do
        isCalculationContainingVariablesTestAux(symbol)
  
    def isCalculationContainingVariablesTestAux(theorySymbol: FunctionSymbol): Unit = {
      def app(term1: Term, term2: Term) = Term.App(theorySymbol, List(term1, term2))
  
      val falseValues: List[Term] = List(
        termFx, x, one, zero, y, z,
        app(x, app(y, termFx)),
        app(termFx, one),
        app(one, one),
        app(one, app(one, one)),
      )
      val trueValues: List[Term] = List(
        app(one, x),
        app(x, one),
        app(x, y),
        app(y, x),
        app(x, app(x, app(x, one)))
      )
      for (t <- falseValues) do
        assertFalse(s"Assert ${t.toPrintString()} is NOT a calculation containing variables.", t.isCalculationContainingVariables)
      for (t <- trueValues) do
        assertTrue(s"Assert ${t.toPrintString()} is a calculation containing variables.", t.isCalculationContainingVariables)
    }
  }

  @org.junit.Test
  def existsTest(): Unit = {
    val bools : List[Boolean] = List()
    assertFalse(bools.exists(identity))
    assertFalse(List(false, false, false).exists(identity))
    assertTrue(List(false, true, false).exists(identity))
    assertTrue(List(true, true, true).exists(identity))
  }
}
