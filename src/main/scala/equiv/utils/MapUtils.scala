package equiv.utils

object MapUtils {
  def union[S,T](map1: Map[S,T], map2: Map[S,T], ignoreConflicts: Boolean = false) : Option[Map[S,T]] = {
    var map = map2
    map1.foreach{ case (k,v) =>
      if(!ignoreConflicts) map.get(k).foreach{ v2 => if(v != v2) return None }
      map = map.updated(k,v)
    }
    Some(map)
  }
}
