package equiv.utils

import equiv.trs.Term.{App, Position, Substitution}
import equiv.trs.{FunctionSymbol, Sort, Term, Typing}
import equiv.utils.TermUtils
import equiv.sample.SampleObjects.{customAddInt, f, four, one, termFx, termFy, two, u, varInt, x, y, z, zero}
import org.junit.Assert.{assertEquals, assertFalse, assertNotEquals, assertTrue}

//noinspection AccessorLikeMethodIsUnit
class TermUtilsTest {

  @org.junit.Test
  def getFreshVarTest(): Unit = {
    assertEquals(0, TermUtils.lastVarNameInt)
    for (i <- 0 to 50) do
      assertEquals(s"v$i", TermUtils.getFreshVarName)
    assertEquals(51, TermUtils.lastVarNameInt)
    assertEquals(Term.Var("v51", Sort.Int), TermUtils.getFreshVar(Sort.Int))
    assertEquals(Term.Var("v52", Sort.Bool), TermUtils.getFreshVar(Sort.Bool))
  }

  @org.junit.Test
  def isOnPathOfTest(): Unit = {
    //noinspection AccessorLikeMethodIsUnit
    def isOnPathOfTestAux(position1: Position, position2: Position, expectedResult: Boolean): Unit = {
      assertEquals(expectedResult, TermUtils.isOnPathOf(position1, position2))
    }
    // Expect true
    isOnPathOfTestAux(List(), List(), true)
    isOnPathOfTestAux(List(), List(1), true)
    isOnPathOfTestAux(List(1), List(), true)
    isOnPathOfTestAux(List(1, 2), List(1), true)
    isOnPathOfTestAux(List(1, 1, 1, 1, 1), List(1), true)
    isOnPathOfTestAux(List(1, 1, 1, 1, 1), List(1, 1, 1, 1, 1, 1, 1, 1, 1), true)
    isOnPathOfTestAux(List(1, 1, 1, 1, 1, 1, 1, 1, 1), List(1, 1, 1, 1, 1), true)
    // Expect false
    isOnPathOfTestAux(List(2), List(1), false)
    isOnPathOfTestAux(List(2, 2, 2, 2, 2), List(2, 2, 2, 2, 0), false)
    isOnPathOfTestAux(List(3, 3, 3), List(2, 2, 2, 2, 2), false)
  }

  @org.junit.Test
  def isIntTest(): Unit = {
    val intValues = List("-1", "-34", "0", "43", "1", Int.MaxValue.toString, Int.MinValue.toString, Long.MaxValue.toString, Long.MinValue.toString)
    for (v <- intValues) do
      assertEquals(true, TermUtils.isInt(v))
    val nonIntValues = List("", "-", "--", "-14-", " ", "blabla", "1x", "1-1-1-1", "3-1=3", "2.5", "0.4", "1/2")
    for (v <- nonIntValues) do
      assertEquals(false, TermUtils.isInt(v))
  }

  @org.junit.Test
  def maybeGetValueTest(): Unit = {
    val intValues = List(0, 1, -1, 2, 232, -34, 13, Int.MinValue, Int.MaxValue)
    for (v <- intValues) do
      assertEquals(Some(App(FunctionSymbol.`Int`(v), List())), TermUtils.maybeGetValue(v.toString))
    val nonIntValues = List("", "-", "--", "-14-", " ", "blabla", "1x", "1-1-1-1", "3-1=3", "2.5", "0.4", "1/2")
    for (v <- nonIntValues) do
      assertEquals(None, TermUtils.maybeGetValue(v))
  }

  @org.junit.Test
  def getRIEqualityFunctionSymbolTest(): Unit = {
    val sorts = List(Sort.Int, Sort.Bool, Sort.Any)
    for (s <- sorts) do
      val result = TermUtils.getRIEqualityFunctionSymbol(s)
      assertEquals(TermUtils.reservedFunctionSymbol, result.name)
      assertEquals(Typing(List(s, s), Sort.Bool), result.typing)
      assertEquals(true, result.isTemporary)
      assertEquals(false, result.isTheory)
      assertEquals(false, result.isValue)
  }

  @org.junit.Test
  def getEqualityFunctionSymbolTest(): Unit = {
    val sorts = List(Sort.Int, Sort.Bool, Sort.Any)
    for (s <- sorts) do
      val result = TermUtils.getEqualityFunctionSymbol //(s)
      assertEquals(TermUtils.equalityFunctionSymbolName, result.name)
      assertEquals(Typing(List(s, s), Sort.Bool), result.typing)
      assertEquals(false, result.isTemporary)
      assertEquals(true, result.isTheory)
      assertEquals(false, result.isValue)
  }

  @org.junit.Test
  def replaceVarInSubTest(): Unit = {
    val var1 = varInt("v1")
    val var2 = varInt("v2")

    val contexts: List[Term => Term] = List(
      t => t,
      t => Term.App(f, List(t)), // f(t)
      t => Term.App(u, List(t, t, t)), // u(t, t, t)
      t => Term.App(u, List(var1, t, t)), // u(t, t, t)
      t => Term.App(f, List(Term.App(u, List(t, Term.App(f, List(t)), one)))) // f(u(t, f(t), 1))
    )
    val replacementTerms = List(zero, two, varInt("w"), termFx, Term.App(customAddInt, List(one, four)))

    for (context <- contexts) do
      for (replacement <- replacementTerms) do
        val substitution = Map(var1 -> context(var2))
        val expected = Map(var1 -> context(replacement))
        val result = TermUtils.replaceVarInSub(var2, replacement, substitution)
        assertEquals(expected, result)

    assertEquals(Map((x, x)), TermUtils.replaceVarInSub(y, x, Map((y, x))))
    assertEquals(Map(), TermUtils.replaceVarInSub(y, x, Map()))
  }

  @org.junit.Test
  def maybeReplaceVarWithVarTest(): Unit = {
    import TermUtils.maybeReplaceVarWithVar

    // Replacement is not done
    assertEquals(x, maybeReplaceVarWithVar(x, y, z))
    assertEquals(x, maybeReplaceVarWithVar(x, z, z))
    assertEquals(x, maybeReplaceVarWithVar(x, z, y))
    assertEquals(x, maybeReplaceVarWithVar(x, y, one))
    assertEquals(x, maybeReplaceVarWithVar(x, y, termFx))

    // Replacement is done
    assertEquals(x, maybeReplaceVarWithVar(x, x, x))
    assertEquals(y, maybeReplaceVarWithVar(x, x, y))
  }

  @org.junit.Test
  def replaceVarInTermPairsTest(): Unit = {
    assertEquals(List(), TermUtils.replaceVarInTermPairs(x, x, List()))
    assertEquals(List((x, x)), TermUtils.replaceVarInTermPairs(x, x, List((x, x))))
    assertEquals(List((x, x)), TermUtils.replaceVarInTermPairs(y, x, List((x, y))))
    assertEquals(List((x, x)), TermUtils.replaceVarInTermPairs(y, x, List((y, x))))
    assertEquals(List((x, x)), TermUtils.replaceVarInTermPairs(y, x, List((y, y))))
    assertEquals(List((one, zero)), TermUtils.replaceVarInTermPairs(x, two, List((one, zero))))

    val longList = (0 to 30).map(_ => (Term.App(f, List(x)), one)).toList
    val expectedLongListResult = (0 to 30).map(_ => (Term.App(f, List(one)), one))
    assertEquals(expectedLongListResult, TermUtils.replaceVarInTermPairs(x, one, longList))
  }

  @org.junit.Test
  def filterVarsTest(): Unit = {
    assertEquals(List(), TermUtils.filterVars(List()))
    assertEquals(List(x), TermUtils.filterVars(List(x)))
    assertEquals(List(x), TermUtils.filterVars(List(x, one)))
    assertEquals(List(), TermUtils.filterVars(List(one)))
    assertEquals(List(), TermUtils.filterVars(List(termFx)))
    assertEquals(List(x, x), TermUtils.filterVars(List(x, x)))
    assertEquals(List(x, y, z, x), TermUtils.filterVars(List(x, y, z, x)))
    assertEquals(List(x, y, z, x), TermUtils.filterVars(List(x, one, termFx, y, one, zero, termFy, z, two, two, two, x)))
  }
}
