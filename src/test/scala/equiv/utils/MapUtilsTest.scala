package equiv.utils

import MapUtils.union
import org.junit.Assert.assertEquals

class MapUtilsTest {
  @org.junit.Test
  def unionTest(): Unit = {
    assertEquals(Some(Map()), union(Map(), Map()))
    assertEquals(None, union(Map(1 -> 2), Map(1 -> 3)))
    assertEquals(Some(Map(1 -> 2)), union(Map(1 -> 2), Map(1 -> 3), true))
    assertEquals(Some(Map(1 -> 2, 2 -> 1)), union(Map(1 -> 2), Map(2 -> 1)))
  }
}
