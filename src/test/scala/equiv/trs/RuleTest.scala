package equiv.trs

import com.sun.xml.internal.bind.v2.runtime.reflect.Lister.Pack
import equiv.utils.{TermUtils, TheorySymbols}
import equiv.sample.SampleObjects.{customAddInt, f, one, termFx, termFy, two, u, x, y, z, zero}
import equiv.trs.Term.Var
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

class RuleTest {
  @org.junit.Test
  def renameVarOccurrencesTest(): Unit = {
    val terms: List[Term] = List(x, y, z, termFx, termFy, Term.App(f, List(x)), Term.App(u, List(x, x, x)), Term.App(u, List(x, y, z)), Term.App(u, List(x, x, y)),
      Term.App(u, List(Term.App(f, List(Term.App(u, List(x, y, zero)))), x, Term.App(u, List(x, y, one)))))
    val variableSets = List(Set(), Set(x), Set(x, y), Set(x, y, z), Set(y, z), Set(x, z))
    val constraintSets = List(Set(), Set(Constraint(TheorySymbols.eqXY(x, y))))
    for (term <- terms) do
      term match {
        case Var(_, _) =>
        case _ =>
          for (term2 <- terms) do
            for (constraintSet <- constraintSets) do
              val rule = Rule(term, term2, constraintSet)
                for (variableSet <- variableSets) do
                  val newRule = rule.renameVarOccurrences(variableSet)
                  assertEquals(s"$newRule does not contain occurrences of $variableSet", Set(), newRule.vars.intersect(variableSet))
      }

    for (term <- terms) do
      term match {
        case Var(_, _) =>
        case _ =>
          for (term2 <- terms) do
            for (constraintSet <- constraintSets)
              val rule = Rule(term, term2, constraintSet)
              for (variableSet <- List(Set(), Set(TermUtils.getFreshVar(Sort.Any)), Set(TermUtils.getFreshVar(Sort.Int)), Set(TermUtils.getFreshVar(x.sort)))) do
                assertEquals(rule, rule.renameVarOccurrences(variableSet))
      }

    val newVar = TermUtils.getFreshVarWithoutIncreasingCounter(x.sort)
    assertEquals(Term.App(u, List(newVar, newVar, newVar)), Term.App(u, List(x, x, x)).renameVarOccurrences(Set(x)))
  }
}
