package equiv.lemma

import com.sun.org.apache.xpath.internal.functions.FuncFalse
import equiv.trs.Term.{App, Var}
import equiv.trs.{Constraint, Rule, Term}

import scala.collection.mutable

class DivergenceChecker {
  var patternSoFar: List[Rule] = List()
  val patternCheckers: List[Checker] = List(
    IdenticalChecker(),
    SkeletonChecker()
  )

  def addRule(rule: Rule): Unit = {
    if patternSoFar.isEmpty then {
      println("First rule added")
      patternSoFar = List(rule)
      return
    }

    var patternDetected = false
    patternCheckers.foreach { patternChecker =>
      val pattern = patternChecker.check(patternSoFar.appended(rule))
      if pattern.nonEmpty then
        println(s"${Console.YELLOW}${patternChecker.name} found a pattern!:${Console.RESET}")
        println(patternSoFar.appended(rule).map(_.toPrintString()).mkString("\n"))
        patternDetected = true
      else
        println(s"${Console.RED}${patternChecker.name} found no pattern.${Console.RESET}")
      println()
    }

    if patternDetected then
      patternSoFar = patternSoFar.appended(rule)
    else
      patternSoFar = List(rule)
  }

  def getLHSRHSvarscounts(rule: Rule): Int = rule.left.vars.size + rule.right.vars.size

  def getVarsInOrder(rule: Rule): List[Term.Var] = List()

  def getMatrix(): Array[Array[String]] = {
    if patternSoFar == List() then return Array()
    val varCount = getLHSRHSvarscounts(patternSoFar.head)
    if !patternSoFar.forall(getLHSRHSvarscounts(_) == varCount) then Array() else {
      Array()
    }
  }


}

type Pattern = Boolean

abstract class Checker {
  val name: String
  def check(rules: List[Rule]): Option[Pattern]
}

/**
 * Checks if the skeletons (so all function symbols) are the same for all rules.
 */
case class SkeletonChecker() extends Checker {
  val name = "Skeleton checker"
  def check(rules: List[Rule]): Option[Pattern] = {
    if rules.sliding(2).map {
      case Seq(rule1, rule2) =>
        checkTermSkeletons(rule1.left, rule2.left) &&
          checkTermSkeletons(rule1.right, rule2.right) &&
          checkSkeletonConstraints(rule1.constraints, rule2.constraints);
      case _ => true
    }.forall(_ == true) then Some(true) else None
  }

  /** For every constraint in constraint set 1: find a skeleton-matching constraint in constraint set 2.
   * If such a constraint does not exists, return false. Otherwise continue without this constraint in constraint set 2.
   * If all constraints from set 1 find a match, then we return true. */
  private def checkSkeletonConstraints(constraints1: Set[Constraint], constraints2: Set[Constraint]): Boolean = {
    val constrains1Queue = mutable.Queue(constraints1.toSeq: _*)
    var constraints2Set = constraints2
    while constrains1Queue.nonEmpty do {
      val constraint1 = constrains1Queue.dequeue()
      val maybeConstraint2 = constraints2Set.find(constraint2 => checkTermSkeletons(constraint1.term, constraint2.term))
      if maybeConstraint2.nonEmpty then
        constraints2Set -= maybeConstraint2.get
      else return false
    }
    true
  }

  /** Check if the skeletons of two terms match (i.e. they are equal up to variable names). */
  private def checkTermSkeletons(term1: Term, term2: Term): Boolean = (term1, term2) match {
    case (App(f1, args1), App(f2, args2)) => (f1.isValue && f2.isValue) || (f1 == f2 && args1.zip(args2).map(checkTermSkeletons).forall(_ == true))
    case (Var(_, _), Var(_, _)) => true
    case _ => false
  }
}

/**
 * Checks if following rules are identical.
 */
case class IdenticalChecker() extends Checker {
  val name = "Identical checker"
  def check(rules: List[Rule]): Option[Pattern] = {
    var same = true
    for (i <- Range(0, rules.length-1)) {
      same = same && rules(i) == rules(i+1)
    }
    if same then Some(true) else None
  }
}

