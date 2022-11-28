package equiv.ri

import equiv.sample.SampleObjects.{eq_, f, four, g, ge_, gt_, le_, lt_, one, three, two, u, v, x, y, z, zero}
import equiv.ri.CALCULATION.removeImpliedConstraints
import equiv.trs.Term.App
import equiv.trs.{Constraint, Term}
import equiv.utils.TermUtils
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
  def findAssignmentsTest() = {
    def check(constraints: Set[Constraint], term: Term) = CALCULATION.getVarsAssignedToTerm(constraints, term)
    val xIs1 = Constraint(App(TermUtils.getEqualityFunctionSymbol, List(x, one)))
    assertEquals(Set(), check(Set(), one))
    assertEquals(Set(x), check(Set(xIs1), one))
    assertNotEquals(Set(x), check(Set(xIs1), x))

  }

}
