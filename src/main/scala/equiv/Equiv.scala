package equiv

import equiv.ri.Rewrite.{rewriteAtPos, *}
import equiv.trs.*
import equiv.trs.Temp.*
import equiv.trs.Term.*
import equiv.trs.Core.boolTrue

object Equiv {
  def main(args: Array[String]): Unit = {
    println("Hello")

    testApp(rho2, consTermFxTrue)
    testApp(rho2, consTermGxTrue)
    testApp(rho2, consTermGFxTrue)
  }

  def testApp(rule: Rule, consTerm: ConstrainedTerm): Unit = {
    println(s"Is rule $rule applicable on term $consTerm?: ${consTerm.term.findSubTermInstances(rule.left).nonEmpty}")
  }

}
