import equiv.utils.TermUtils
import org.junit.Assert.assertEquals

class TermUtilsTest {

  @org.junit.Test
  def test1(): Unit = {
    val input1 = List()
    val input2 = List()
    val result = TermUtils.isOnPathOf(input1, input2)
    val expectedResult = true
    assertEquals(expectedResult, result)
  }
}
