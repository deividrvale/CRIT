package equiv.sample

import equiv.ri.Equation
import equiv.trs.Term.{App, Var}
import equiv.trs.*
import equiv.utils.TheorySymbols.*

object SampleObjects {
  /* VALUES */
  val zero: Term = valInt(0)
  val one: Term = valInt(1)
  val two: Term = valInt(2)
  val three: Term = valInt(3)
  val four: Term = valInt(4)

  /* VARIABLES */
  val x: Var = varInt("x")
  val y: Var = varInt("y")
  val i: Var = varInt("i")
  val z: Var = varInt("z")

  /* FUNCTION SYMBOLS */
  val f: FunctionSymbol = funcIntInt("f", false)
  val g: FunctionSymbol = funcIntInt("g", false)
  val returnInt: FunctionSymbol = funcIntInt("return", false)
  val u: FunctionSymbol = funcIntIntIntInt("u", false)
  val v: FunctionSymbol = funcIntIntInt("v", false)
  val customAddInt: FunctionSymbol = funcIntIntInt("add", false).copy(infix = None)
  val sumUp: FunctionSymbol = funcIntInt("sumup", false)
  val sumDown: FunctionSymbol = funcIntInt("sumdown", false)
  val sumRec: FunctionSymbol = funcIntInt("sumrec", false)
//  val functionSymbolGT: FunctionSymbol = funcIntIntBool(">")
//  val functionSymbolLT: FunctionSymbol = funcIntIntBool("<")
//  val functionSymbolGE: FunctionSymbol = funcIntIntBool(">=")
//  val functionSymbolLE: FunctionSymbol = funcIntIntBool("<=")

  /* TERMS */
  val termFx: App = App(f, List(x))
  val termFy: App = App(f, List(y))
  val termGx: App = App(g, List(x))
  val termGy: App = App(g, List(y))

  /* TERM FUNCTIONS */
  def return1(term: Term): App = App(funcIntInt("return1", false), List(term))
  def return2(term: Term): App = App(funcIntInt("return2", false), List(term))
  def gt_(term: Term, term2: Term): App = App(gt, List(term, term2))
  def lt_(term: Term, term2: Term): App = App(lt, List(term, term2))
  def ge_(term: Term, term2: Term): App = App(ge, List(term, term2))
  def le_(term: Term, term2: Term): App = App(le, List(term, term2))
  def eq_(term: Term, term2: Term): App = App(eql, List(term, term2))

  /*            RULES                */
  val rule1: Rule = Rule(termFx, App(f, List(App(min, List(x, one)))), Set(Constraint(App(gt, List(x, zero)))))
  val rule2: Rule = Rule(termFx, App(returnInt, List(zero)), Set(Constraint(App(le, List(x, zero)))))
  val rule3: Rule = Rule(termFx, App(f, List(App(min, List(x, one)))), Set(Constraint(App(and, List(App(gt, List(x, zero)), App(not, List(App(eql, List(x, three)))))))))
  val rule4: Rule = Rule(termFx, App(f, List(one)), Set(Constraint(App(eql, List(x, three)))))

  val sumUpRules: Set[Rule] = Set(
    Rule(App(sumUp, List(x)), App(u, List(x, one, zero)), Set()),
    Rule(makeAppTer(u, x, i, z), makeAppTer(u, x, makeAppBin(add, i, one), makeAppBin(add, z, i)), Set(makeConsBin(i, le, x))),
    Rule(makeAppTer(u, x, i, z), makeAppUn(returnInt, z), Set(makeConsBin(i, gt, x)))
  )
  val sumRecRules: Set[Rule] = Set(
    Rule(makeAppUn(sumRec, x), makeAppBin(customAddInt, x, makeAppUn(sumRec, makeAppBin(min, x, one))), Set(makeConsBin(x, gt, zero))),
    Rule(makeAppUn(sumRec, x), makeAppUn(returnInt, zero), Set(makeConsBin(x, le, zero))),
    Rule(makeAppBin(customAddInt, x, makeAppUn(returnInt, y)), makeAppUn(returnInt, makeAppBin(add, x, y)), Set())
  )
  val sumDownRules: Set[Rule] = Set(
    Rule(makeAppUn(sumDown, x), makeAppBin(v, x, zero), Set()),
    Rule(makeAppBin(v, x, z), makeAppBin(v, makeAppBin(min, x, one), makeAppBin(add, z, x)), Set(makeConsBin(x, gt, zero))),
    Rule(makeAppBin(v, x, z), makeAppUn(returnInt, z), Set(makeConsBin(x, le, zero)))
  )

  /*              EQUATIONS                */
  val constructorEquation: Equation = Equation(
    App(returnInt, List(zero)),
    App(returnInt, List(x)),
    Set(Constraint(App(eql, List(x, zero))))
  )

  val deletionEquation1: Equation = Equation(termFy, termFy, Set())
  val deletionEquation2: Equation = Equation(termFy, termGy, Set(consVarIntInt("y", ">", 1), consVarIntInt("y", "<", 1)))

  val disproveEquation1: Equation = Equation(one, zero, Set())
  val disproveEquation2: Equation = Equation(return1(x), return2(x), Set(Constraint(App(le, List(x, two)))))
  val disproveEquation3: Equation = Equation(x, return1(x), Set())
  val disproveEquation4: Equation = Equation(x, y, Set())

  val expansionEquation: Equation = Equation(termFx, App(f, List(four)), Set())

  val eqDelEq: Equation = Equation(makeAppUn(returnInt, makeAppTer(u, x, x, x)), makeAppUn(returnInt, makeAppTer(u, y, y, y)), Set(makeConsBin(x, eql, y)))
  val eqDelEq2: Equation = Equation(makeAppBin(add, one, one), makeAppBin(add, zero, one), Set())
  val eqDelEq5: Equation = Equation(makeAppUn(returnInt, makeAppBin(add, x, one)), makeAppUn(returnInt, makeAppBin(add, y, one)), Set(Constraint(makeAppUn(not, makeAppBin(eql, x, y)))))
  val eqDelEq3: Equation = Equation(makeAppUn(returnInt, makeAppBin(add, zero, one)), makeAppUn(returnInt, makeAppBin(add, one, one)), Set())
  val eqDelEq4: Equation = Equation(makeAppBin(add, x, one), makeAppBin(add, y, one), Set())
  val eqDelEq6: Equation = Equation(makeAppTer(u, x, makeAppBin(add, x, one), makeAppBin(min, x, one)), makeAppTer(u, y, makeAppBin(add, x, two), makeAppBin(add, y, one)), Set())

  val disProveEq2: Equation = Equation(makeAppUn(returnInt, x), makeAppUn(returnInt, y), Set(Constraint(makeAppUn(not, makeAppBin(eql, x, y)))))

  val sumUpRecEq: Equation = Equation(makeAppUn(sumUp, x), makeAppUn(sumRec, x), Set())
  val sumUpDownEq: Equation = Equation(makeAppUn(sumUp, x), makeAppUn(sumDown, x), Set())
  val sumDownRecEq: Equation = Equation(makeAppUn(sumDown, x), makeAppUn(sumRec, x), Set())

  val sumUpRec1: Equation = Equation(makeAppUn(returnInt, zero), makeAppUn(sumRec, x), Set(makeConsBin(one, gt, x)))
  val sumUpRec2: Equation = Equation(makeAppUn(returnInt, zero), makeAppUn(sumRec, y), Set(makeConsBin(one, gt, y)))

  val fRule: Rule = Rule(makeAppUn(f, one), makeAppUn(returnInt, two), Set())
  val fRule2: Rule = Rule(makeAppUn(f, two), makeAppUn(returnInt, one), Set())
  val expEq1: Equation = Equation(makeAppUn(f, x), makeAppUn(returnInt, two), Set())
  val expEq2: Equation = Equation(makeAppUn(f, x), makeAppUn(returnInt, makeAppBin(add, one, one)), Set())

  /* Functions for constructing objects */

  /** @return A function symbol of type [Int x Int] => Bool */
  def funcIntIntBool(operator: String, isTheory: Boolean = true, isValue: Boolean = false): FunctionSymbol =
    FunctionSymbol(operator, Typing(List(Sort.Int, Sort.Int), Sort.Bool), isTheory, isValue, Some(Infix(InfixKind.Right, 1)))

  def funcIntInt(operator: String, isTheory: Boolean = true, isValue: Boolean = false): FunctionSymbol =
    FunctionSymbol(operator, Typing(List(Sort.Int), Sort.Int), isTheory, isValue, Some(Infix(InfixKind.Right, 1)))

  def funcIntIntIntInt(operator: String, isTheory: Boolean = true, isValue: Boolean = false): FunctionSymbol =
    FunctionSymbol(operator, Typing(List(Sort.Int, Sort.Int, Sort.Int), Sort.Int), isTheory, isValue, Some(Infix(InfixKind.Right, 1)))

  /** @return A function symbol of type [Bool x Bool] => Bool */
  def funcBoolBoolBool(operator: String, isTheory: Boolean = true, isValue: Boolean = false): FunctionSymbol =
    FunctionSymbol(operator, Typing(List(Sort.Bool, Sort.Bool), Sort.Bool), isTheory, isValue, Some(Infix(InfixKind.Right, 1)))

  /** @return An integer value */
  def valInt(value: Int): Term =
    App(FunctionSymbol.`Int`(value), List())

  /** @return An integer variable */
  def varInt(name: String): Var = Var(name, Sort.Int)

  /** A constraint for an integer variable and an integer value. */
  def consVarIntInt(variableName: String, operatorName: String, value: Int): Constraint =
    makeConsBin(varInt(variableName), funcIntIntBool(operatorName), valInt(value))

  def consVarIntInt2(variableName: String, operatorName: String, value: Int): Constraint =
    makeConsBin(App(funcIntIntInt("-"), List(varInt(variableName), valInt(1))), funcIntIntBool(operatorName), valInt(value - 1))

  def makeConsBin(arg1: Term, fun: FunctionSymbol, arg2: Term): Constraint =
    Constraint(App(fun, List(arg1, arg2)))

  def makeAppBin(functionSymbol: FunctionSymbol, term1: Term, term2: Term): App = {
    App(functionSymbol, List(term1, term2))
  }

  def makeAppTer(functionSymbol: FunctionSymbol, term1: Term, term2: Term, term3: Term): App = {
    App(functionSymbol, List(term1, term2, term3))
  }

  def makeAppUn(functionSymbol: FunctionSymbol, term1: Term): App = {
    App(functionSymbol, List(term1))
  }

}
