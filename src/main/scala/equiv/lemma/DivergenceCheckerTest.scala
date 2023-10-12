package equiv.lemma

import equiv.InputHandler
import equiv.trs.{Constraint, FunctionSymbol, Rule, Sort, Term, Typing}

import scala.collection.mutable
import scala.io.StdIn.readLine

object DivergenceCheckerTest {
  def main(args: Array[String]): Unit = {
    hardcodedInput()
  }

  val simpleRulesToAdd: mutable.Queue[Rule] = mutable.Queue(
    ruleSkeleton(getTerm("rr", List()), getVar("x")),
    ruleSkeleton(getTerm("rr", List()), getVar("xx")),
  )

  val rulesToAdd: mutable.Queue[Rule] = mutable.Queue(
    getRule(Set(eqConstraint("x", 1))),
    getRule(Set(eqConstraint("x", 1))),
    getRule(Set(eqConstraint("x", 1))),
    getRule(Set(eqConstraint("x", 1), eqConstraint("x2", 2))),
    getRule(Set(eqConstraint("x", 1), eqConstraint("x2", 3)))
  )

  def hardcodedInput(): Unit = {
    val dc = DivergenceChecker()

    while (rulesToAdd.nonEmpty) {
      val rule = rulesToAdd.dequeue()
      dc.addRule(rule)
      readLine()
    }

  }

  def manualInput(): Unit = {
    val dc = DivergenceChecker()
    while(true) {
      print("\n\n-----------------------------------------------------------\n\nEnter two constants, separated by a comma: ")
      val maybeRule = makeRule(readLine().filterNot(_.isWhitespace))
      if maybeRule.nonEmpty then { println(s"Found ${maybeRule.get.toPrintString()}") ; dc.addRule(maybeRule.get) } else { }
    }
  }

  def makeRule(input: String): Option[Rule] = {
    val args = input.split(',')
    if args.length != 2 then { None } else {
      Some(Rule(
        Term.App(FunctionSymbol(args(0), Typing(List(), Sort.Int)), List()),
        Term.App(FunctionSymbol(args(1), Typing(List(), Sort.Int)), List()),
        Set()
      ))
    }
  }


  def eqConstraint(varName: String, value: Int): Constraint = Constraint(getTheoryTerm("=", List(getValue(value), getVar(varName))))
  def gtConstraint(varName: String, value: Int): Constraint = Constraint(getTheoryTerm(">", List(getValue(value), getVar(varName))))

  def ruleSkeleton(arg1: Term, arg2: Term): Rule = Rule(getTerm("f", List(arg1, arg2)), getTerm("g", List(getTerm("r", List()))), Set())
  def getRule(constraints: Set[Constraint]): Rule = Rule(getTerm("r-left", List()), getTerm("r-right", List()), constraints)

  def getVar(name: String): Term.Var = Term.Var(name, Sort.Int)
  def getTerm(name: String, args: List[Term]): Term = Term.App(f(name, args.size), args)
  def getTheoryTerm(name: String, args: List[Term]): Term = Term.App(f(name, args.size, true), args)
  def f(name: String, args: Int, theory: Boolean = false): FunctionSymbol = FunctionSymbol(name, Typing(List.fill(args)(Sort.Int), if theory then Sort.Bool else Sort.Int), theory)
  def getValue(i: Int): Term = Term.App(FunctionSymbol(i.toString, Typing(List(), Sort.Int), true, true), List())
}
