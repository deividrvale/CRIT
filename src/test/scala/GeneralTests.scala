import org.junit.Assert.assertEquals

class GeneralTests {
  @org.junit.Test
  def mapBehaviourTest(): Unit = {
    assertEquals(Map((1,1)), Map(1 -> 1))
    assertEquals(Map(1 -> 1), Map(1 -> 1).updated(1, 1))
    assertEquals(Map(1 -> 2), Map(1 -> 3).updated(1, 2))
  }
}
