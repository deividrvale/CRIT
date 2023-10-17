package equiv.lemma

import com.sun.org.apache.xpath.internal.functions.FuncFalse
import equiv.trs.Term.{App, Var}
import equiv.trs.{Constraint, Rule, Term}

import java.io.{File, FileWriter, PrintWriter}
import scala.collection.mutable

class DivergenceChecker() {
  var patternSoFar: List[Rule] = List()
  val patternCheckers: List[Checker] = List(
    IdenticalChecker(),
    SkeletonChecker()
  )

  def addRule(rule: Rule): Unit = {
    if patternSoFar.isEmpty then {
      printResult("First rule added\n")
      patternSoFar = List(rule)
      return
    }

    var patternDetected = false
    patternCheckers.foreach { patternChecker =>
      val pattern = patternChecker.check(patternSoFar.appended(rule))
      if pattern.nonEmpty then
        printResult(s"${Console.YELLOW}${patternChecker.name} found a pattern!:${Console.RESET}\n")
//        printResult(patternSoFar.appended(rule).map(_.toPrintString()).mkString("","\n","\n"))
        printResult(patternChecker.printPattern(patternSoFar.appended(rule)))
        patternDetected = true
      else
        printResult(s"${Console.RED}${patternChecker.name} found no pattern.${Console.RESET}\n")
      printResult("\n")
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

  def printResult(string: String): Unit = {
    print(string)
  }
}

abstract class Checker {
  val name: String
  type Pattern
  def check(rules: List[Rule]): Option[Pattern]
  def getPattern(rules: List[Rule]): Pattern
  def printPattern(rules: List[Rule]): String
}

/**
 * Checks if the skeletons (so all function symbols) are the same for all rules.
 */
case class SkeletonChecker() extends Checker {
  override type Pattern = Array[Array[String]]

  val name = "Skeleton checker"
  def check(rules: List[Rule]): Option[Pattern] = {
    if rules.sliding(2).map {
      case Seq(rule1, rule2) =>
        checkTermSkeletons(rule1.left, rule2.left) &&
          checkTermSkeletons(rule1.right, rule2.right) &&
          checkSkeletonConstraints(rule1.constraints, rule2.constraints);
      case _ => true
    }.forall(_ == true) then Some(getPattern(rules)) else None
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

  /** Check if the skeletons of two terms match (i.e. they are equal up to variable names and values). */
  private def checkTermSkeletons(term1: Term, term2: Term): Boolean = (term1, term2) match {
    case (App(f1, args1), App(f2, args2)) => (f1.isValue && f2.isValue) || (f1 == f2 && args1.zip(args2).map(checkTermSkeletons).forall(_ == true))
    case (Var(_, _), Var(_, _)) => true
    case (Var(_, _), App(f2, _)) => f2.isValue
    case (App(f1, _), Var(_, _)) => f1.isValue
    case _ => false
  }

  override def getPattern(rules: List[Rule]): Pattern = {
    // get vars from LHS and RHS
    // for every var, get the value from the constraint and put in array
    def getVarValue(rule: Rule, v: Var): String = {
      val terms: Set[Term] = rule.constraints.flatMap(constraint => constraint.term.getTermsAssignedToVar(v))
      if terms.isEmpty then v.toString else terms.mkString(" && ")
    }

    rules.map { rule =>
      val varsValsInOrder = rule.getRuleLRHSVarsValsInOrder
      varsValsInOrder.map { case v@Var(_, _) => getVarValue(rule, v); case x => x.toString }.toArray
    }.toArray
  }

  override def printPattern(rules: List[Rule]): String = {
    if (rules.isEmpty) {
      throw Error("Something went wrong. No rules in pattern.")
    }
    val pattern: Array[Array[String]] = getPattern(rules)
    val maxColumnWidths: Array[Int] = Array.fill(pattern.head.length)(0)
    for (row <- pattern) {
      for (i <- Range(0, row.length)) {
        maxColumnWidths(i) =  maxColumnWidths(i).max(row(i).length)
      }
    }
    var returnString = ""
    pattern.foreach(array =>
      for(i <- Range(0, array.length)) {
        returnString += s"${array(i)}  ${(1 to maxColumnWidths(i) - array(i).length).flatMap(_ => " ").mkString("")}"
      }
      returnString += "\n"
    )
    returnString
  }
}

/**
 * Checks if following rules are identical.
 */
case class IdenticalChecker() extends Checker {
  override type Pattern = String
  val name = "Identical checker"
  def check(rules: List[Rule]): Option[Pattern] = {
    var same = true
    for (i <- Range(0, rules.length-1)) {
      same = same && rules(i) == rules(i+1)
    }
    if same then Some(rules.head.toString) else None
  }

  override def getPattern(rules: List[Rule]): Pattern = rules.head.toPrintString()
  override def printPattern(rules: List[Rule]): String = rules.size + " times: " + getPattern(rules)
}

