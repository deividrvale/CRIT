package tests

import SampleObjects.*
import equiv.utils.TermUtils.*
import tests.SampleObjects.Values.*
import SampleObjects.Sums.*
import equiv.trs.Term.Substitution

object TermUtilsTests {

  def main(): Unit = {
    testMaybeReverseMap(Map((x, z)), Some(Map((z, x))))
    testMaybeReverseMap(Map((x, one)), None)
    testMaybeReverseMap(Map((x, y), (z, y)), None)
  }

  def testMaybeReverseMap(substitution: Substitution, expectedOutPut: Option[Substitution]): Unit = {
    assert(maybeReverseMap(substitution) == expectedOutPut)
    println(s"Assertion success: maybeReverse of $substitution is $expectedOutPut")
  }
}
