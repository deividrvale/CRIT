package equiv.ri

import equiv.sample.SampleObjects.{eq_, f, four, g, ge_, gt_, le_, lt_, one, three, two, u, v, x, y, z, zero}
import equiv.ri.CALCULATION.removeImpliedConstraints
import equiv.trs.Term.App
import equiv.trs.{Constraint, Term}
import equiv.utils.{TermUtils, TheorySymbols}
import equiv.utils.TheorySymbols.add
import org.junit.Assert.{assertEquals, assertNotEquals, assertTrue}

class CALCULATIONTest {
  @org.junit.Test
  def removeImpliedConstraintsTest(): Unit = {
    val gt_x0 = Constraint(gt_(x, zero))
    val gt_x1 = Constraint(gt_(x, one))
    val gt_x2 = Constraint(gt_(x, two))
    assertEquals(Set(), removeImpliedConstraints(Set()))
    assertEquals(Set(gt_x0), removeImpliedConstraints(Set(gt_x0)))
    assertEquals(Set(gt_x1), removeImpliedConstraints(Set(gt_x0, gt_x1)))
    assertEquals(Set(gt_x2), removeImpliedConstraints(Set(gt_x0, gt_x1, gt_x2)))

    val ge_x0 = Constraint(ge_(x, zero))
    val ge_x1 = Constraint(ge_(x, one))
    val ge_x2 = Constraint(ge_(x, two))
    val res1 = removeImpliedConstraints(Set(ge_x1, gt_x0))
    assertEquals(1, res1.size)
    assert(res1 == Set(ge_x1) || res1 == Set(gt_x0))

    val lt_x4 = Constraint(lt_(x, four))
    val lt_x3 = Constraint(lt_(x, three))
    val lt_x2 = Constraint(lt_(x, two))
    val lt_x1 = Constraint(lt_(x, one))

    val le_x4 = Constraint(le_(x, four))
    val le_x3 = Constraint(le_(x, three))
    val le_x2 = Constraint(le_(x, two))
    val le_x1 = Constraint(le_(x, one))
    val le_x0 = Constraint(le_(x, zero))

    val eq_c_1 = Constraint(eq_(x, one))

//    assertEquals(Set(eq_c_1), removeImpliedConstraints(Set(le_x1, ge_x1, eq_c_1)))
//    assertEquals(Set(eq_c_1), removeImpliedConstraints(Set(eq_c_1, le_x1, ge_x1)))
    assertTrue(removeImpliedConstraints(Set(eq_c_1, le_x1, ge_x1)).size < 3)

    val css = Set(gt_x0, gt_x1, gt_x2)
    assertEquals(css, removeImpliedConstraints(Set(), css))
  }

  @org.junit.Test
  def getVarsAssignedToTermTest() = {
    def getVarsAssignedToTerm(constraints: Set[Constraint], term: Term) = CALCULATION.getVarsAssignedToTerm(constraints, term)
    val xIs1 = Constraint(App(TermUtils.getEqualityFunctionSymbol, List(x, one)))
    val xIs2 = Constraint(App(TermUtils.getEqualityFunctionSymbol, List(x, two)))
    val xIs1And2 = Constraint(App(TermUtils.getEqualityFunctionSymbol, List(x, one, two)))
    val xAndyIs1And2 = Constraint(App(TermUtils.getEqualityFunctionSymbol, List(x, one, y, two)))
    assertEquals(Set(), getVarsAssignedToTerm(Set(), one))
    assertEquals(Set(x), getVarsAssignedToTerm(Set(xIs1), one))
    assertNotEquals(Set(x), getVarsAssignedToTerm(Set(xIs1), x))
    assertEquals(Set(x), getVarsAssignedToTerm(Set(xIs1, xIs2), one))
    assertEquals(Set(x), getVarsAssignedToTerm(Set(xIs1, xIs2), two))

    assertEquals(Set(x), getVarsAssignedToTerm(Set(xIs1And2), one))
    assertEquals(Set(x), getVarsAssignedToTerm(Set(xIs1And2), two))
    assertEquals(Set(x, y), getVarsAssignedToTerm(Set(xAndyIs1And2), one))
    assertEquals(Set(x, y), getVarsAssignedToTerm(Set(xAndyIs1And2), two))
  }

  @org.junit.Test
  def getSubtermVarReplacementEquationsTest(): Unit = {
    val inputExpectedOutputPairs: List[(Set[Equation], Set[Equation])] = getCalcEqs()
    for (pair <- inputExpectedOutputPairs) do
      val pfSt = new ProofState(pair._1, Set())
      val result = CALCULATION.getSubtermVarReplacementEquations(pfSt)
      assertEquals(pair._2, result.toSet)
  }

  /** Get a list of pairs where the left element is a set of equations and the right element the maximum subset of the left,
   * such that every element in right is a */
  def getCalcEqs() : List[(Set[Equation], Set[Equation])] = {
    import equiv.sample.SampleObjects.{makeAppBin, makeAppUn}
    import equiv.utils.TheorySymbols.eql

    def makeCalcEq(left: Term, right: Term) = Equation(left, right, Set())
    def makeCalcEqCons(left: Term, right: Term, cons: Term) = Equation(left, right, Set(Constraint(cons)))

    val calcReplaceEq1 = makeCalcEq(makeAppUn(f, makeAppBin(add, x, one)), zero)
    val calcReplaceEq2 = makeCalcEqCons(makeAppUn(f, makeAppBin(add, x, one)), zero, makeAppBin(eql, y, makeAppBin(add, x, one)))
    val calcReplaceEq3 = makeCalcEqCons(makeAppUn(f, makeAppBin(add, x, one)), zero, makeAppBin(eql, y, makeAppBin(add, one, x)))
    val calcReplaceEq4 = makeCalcEq(makeAppUn(f, makeAppBin(add, x, one)), makeAppUn(f, makeAppBin(add, x, one)))
    val calcReplaceEq5 = makeCalcEqCons(makeAppUn(f, makeAppBin(add, x, one)), makeAppUn(f, makeAppBin(add, x, one)), makeAppBin(eql, y, makeAppBin(add, x, one)))
    val calcReplaceEq6 = makeCalcEqCons(makeAppUn(f, makeAppBin(add, x, one)), makeAppUn(f, makeAppBin(add, x, one)), makeAppBin(eql, y, makeAppBin(add, one, x)))

    val nonCalcReplaceEq1 = makeCalcEq(one, zero)
    val nonCalcReplaceEq2 = makeCalcEq(makeAppBin(add, one, one), one)

    val calcReplaceEqs = Set(calcReplaceEq1, calcReplaceEq2, calcReplaceEq3, calcReplaceEq4, calcReplaceEq5, calcReplaceEq6)
    val nonCalcReplaceEqs = Set(nonCalcReplaceEq1, nonCalcReplaceEq2)

    // Make list [ (input 1, expected output 1), ..., (input n, expected output n)  ]
    List(
      (Set(), Set()),
      (Set(calcReplaceEq1), Set(calcReplaceEq1)),
      (Set(calcReplaceEq2), Set(calcReplaceEq2)),
      (Set(calcReplaceEq3), Set(calcReplaceEq3)),
      (Set(calcReplaceEq4), Set(calcReplaceEq4)),
      (Set(calcReplaceEq5), Set(calcReplaceEq5)),
      (Set(calcReplaceEq6), Set(calcReplaceEq6)),
      (Set(nonCalcReplaceEq1), Set()),
      (Set(nonCalcReplaceEq2), Set()),
      (Set(calcReplaceEq1, nonCalcReplaceEq1), Set(calcReplaceEq1)),
      (calcReplaceEqs ++ nonCalcReplaceEqs, calcReplaceEqs)
    )
  }
}
