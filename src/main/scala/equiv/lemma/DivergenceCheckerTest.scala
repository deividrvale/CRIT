package equiv.lemma

import equiv.InputHandler
import equiv.trs.{Constraint, FunctionSymbol, Infix, Rule, Sort, Term, Typing, InfixKind}

import java.io.PrintWriter
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
    ruleSkeleton(getVar("x"), getVar("x1"), Set(eqConstraint2("x1", "x", 1))),
    ruleSkeleton(getVar("x"), getVar("x1"), Set(eqConstraint2("x1", "x", 1))),
    ruleSkeleton(getVar("x"), getVar("x1"), Set(eqConstraint2("x1", "x", 1))),
    ruleSkeleton(getVar("x"), getVar("x2"), Set(eqConstraint2("x2", "x", 2))),
    ruleSkeleton(getVar("x"), getVar("x3"), Set(eqConstraint2("x3", "x", 3), eqConstraint("x3", 2))),
    ruleSkeleton(getVar("x"), getVar("x3"), Set(eqConstraint2("x4", "x", 4), eqConstraint("x4", -100))),
    ruleSkeleton(getVar("x"), getVar("x3"), Set(eqConstraint2("x5", "x", 5), eqConstraint("x5", 4), eqConstraint("x", 30)))
  )

  def hardcodedInput(): Unit = {
    val dc = DivergenceChecker()

    while (rulesToAdd.nonEmpty) {
      val rule = rulesToAdd.dequeue()
      dc.addRule(rule)
      print("Press enter")
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


  def eqConstraint(varName: String, value: Int): Constraint = Constraint(getTheoryTermBool("=", List(getValue(value), getVar(varName))))
  def eqConstraint2(varName: String, varName2: String, value: Int): Constraint = Constraint(getTheoryTermBool("=", List(getAdd(varName2, value), getVar(varName))))
  def gtConstraint(varName: String, value: Int): Constraint = Constraint(getTheoryTermBool(">", List(getValue(value), getVar(varName))))

  def getAdd(varName: String, value: Int): Term = getTheoryTermInt("+", List(getVar(varName), getValue(value)))

  def ruleSkeleton(arg1: Term, arg2: Term, constraints: Set[Constraint] = Set()): Rule = Rule(getTerm("f", List(arg1, arg2)), getTerm("g", List(getTerm("r", List()))), constraints)
  def getRule(constraints: Set[Constraint]): Rule = Rule(getTerm("f", List()), getTerm("g", List()), constraints)

  def getVar(name: String): Term.Var = Term.Var(name, Sort.Int)
  def getTerm(name: String, args: List[Term]): Term = Term.App(f(name, args.size), args)
  def getTheoryTermBool(name: String, args: List[Term]): Term = Term.App(f(name, args.size, true), args)
  def getTheoryTermInt(name: String, args: List[Term]): Term = Term.App(f(name, args.size, true, true), args)
  def f(name: String, args: Int, theory: Boolean = false, intOut: Boolean = false): FunctionSymbol = FunctionSymbol(name, Typing(List.fill(args)(Sort.Int), if theory && !intOut then Sort.Bool else Sort.Int), theory, false, if theory then Some(Infix(InfixKind.Chain, 3)) else None )
  def getValue(i: Int): Term = Term.App(FunctionSymbol(i.toString, Typing(List(), Sort.Int), true, true), List())
}
