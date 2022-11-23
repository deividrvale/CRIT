import org.junit.Assert.{assertEquals, assertNotEquals}
import equiv.trs.Term
import equiv.trs.Term.Substitution

class MatchAndUnificationTests {
  @org.junit.Test
  def unificationTest(): Unit = {
    import equiv.sample.SampleObjects.{u, v, x, y, z, f, g, termFx, zero, one, two}

    checkMultipleUnifiability(
      x,
      List((one, Map(x -> one)), (x, Map()), (y, Map(x -> y))),
      List(termFx)
    )

    checkMultipleUnifiability(
      Term.App(u, List(x, x, y)),
      List((Term.App(u, List(one, one, one)), Map(x -> one, y -> one))),
      List(one, x, zero, Term.App(f, List(z)), Term.App(f, List(x)), Term.App(u, List(one, zero, one)))
    )

    def checkMultipleUnifiability(term: Term, unifiableTermSubstitutionPairs: List[(Term, Substitution)], nonUnifiableTerms: List[Term]): Unit = {
      for ((t, s) <- unifiableTermSubstitutionPairs) do
        assertEquals(Some(s), term.unifiableWith(t))
      for (t <- nonUnifiableTerms) do
        assertEquals(None, term.unifiableWith(t))
    }

    val t1 = Term.App(u, List(x, y, z))
    val t2 = Term.App(u, List(x, x, y))
    assertEquals(Some(Map(y -> x, z -> x)), t1.unifiableWith(t2))
    assertEquals(Some(Map(x -> z, y -> z)), t2.unifiableWith(t1))

    val t3 = Term.App(v, List(x, y))
    val t4 = Term.App(v, List(y, x))
    val map1 = t3.unifiableWith(t4)
    val map2 = t4.unifiableWith(t3)
    assertEquals(map1, map2.map(s => s.map( (a, b) => (b, a) )))
    assertEquals(Some(Map(x -> y)), t3.unifiableWith(t4))
    assertEquals(Some(Map(y -> x)), t4.unifiableWith(t3))

    val t5 = Term.App(u, List(x, x, x))
    val t6 = Term.App(u, List(z, one, one))
    assertEquals(Some(Map(x -> one, z -> one)), t5.unifiableWith(t6))

    // Term Rewriting Systems - Terese: Exercise 2.1.22 (i) - (iii) (where behind the scenes: H = v, F = f, G = g)
    def H(t1: Term, t2: Term): Term = Term.App(v, List(t1, t2))
    def F(t1: Term): Term = Term.App(f, List(t1))
    def G(t1: Term): Term = Term.App(g, List(t1))
    val ex_i1 = H(x, H(x, F(y))) ; val ex_i2 = H(H(G(y),y),z)
    val ex_ii1 = H(H(x, y), x) ; val ex_ii2 = H(H(F(y), z), G(z))
    val ex_iii1 = H(H(G(x), F(y)), F(z)) ; val ex_iii2 = H(H(G(z), F(F(x))), F(y))
    assertNotEquals(None, ex_i1.unifiableWith(ex_i2))
    assertNotEquals(None, ex_i2.unifiableWith(ex_i1))
    assertEquals(None, ex_ii1.unifiableWith( ex_ii2 ))
    assertEquals(None, ex_ii2.unifiableWith( ex_ii1 ))
    assertEquals(None, ex_iii1.unifiableWith(ex_iii2))
    assertEquals(None, ex_iii2.unifiableWith(ex_iii1))
  }
}
