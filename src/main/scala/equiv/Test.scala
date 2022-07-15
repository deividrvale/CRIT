package equiv

import equiv.ri.Simplify
import equiv.trs.Temp.{consVarIntInt, termFx, termFxMinOne, termFy, valZero, xMinusOne}
import equiv.trs.Term.App
import equiv.utils.TermUtils
import equiv.trs.Temp.SumUp.*

object Test {
  def main(args: Array[String]): Unit = {
    val term1 = App(u, List(x, App(returnf, List(x)), valZero))
    val term2 = App(u, List(x, y, z))
    println(s"$term1 instance of $term2?\n${term1.instanceOf(term2)}")

    println(!true || false)
  }
}
