package equiv.trs
import equiv.trs.Term.{App, Var}
import equiv.ri.Equation
import equiv.trs.System
import equiv.trs.Temp.SumUp.*
import equiv.trs.Temp.Values.*
import equiv.utils.TheorySymbols.*

object Temp {
  object Values {
    val zero: Term = valInt(0)
    val one: Term = valInt(1)
    val two: Term = valInt(2)
    val three: Term = valInt(3)
    val four: Term = valInt(4)
  }

  object InferenceRuleEquations {
    val f: FunctionSymbol = funcIntInt("f", false)
    val g: FunctionSymbol = funcIntInt("g", false)
    val termFx: App = App(f, List(x))
    val termFy: App = App(f, List(y))
    val termGx: App = App(g, List(x))
    val termGy: App = App(g, List(y))
    def return1(term: Term): App = App(funcIntInt("return1", false), List(term))
    def return2(term: Term): App = App(funcIntInt("return2", false), List(term))

    val rule1: Rule = Rule(termFx, App(f, List(App(min, List(x, one)))), Set(Constraint(App(gt, List(x, zero)))))
    val rule2: Rule = Rule(termFx, App(returnf, List(zero)), Set(Constraint(App(le, List(x, zero)))))
    val rule3: Rule = Rule(termFx, App(f, List(App(min, List(x, one)))), Set(Constraint(App(and, List(App(gt, List(x, zero)), App(not, List(App(eql, List(x, three)))))))))
    val rule4: Rule = Rule(termFx, App(f, List(one)), Set(Constraint(App(eql, List(x, three)))))

    val constructorEquation: Equation = Equation(
      App(returnf, List(zero)),
      App(returnf, List(x)),
      Set(Constraint(App(eql, List(x, zero))))
    )

    val deletionEquation1: Equation = Equation(termFy, termFy, Set())
    val deletionEquation2: Equation = Equation(termFy, termGy, Set(consVarIntInt("y", ">", 1), consVarIntInt("y", "<", 1)))

    val disproveEquation1: Equation = Equation(one, zero, Set())
    val disproveEquation2: Equation = Equation(return1(x), return2(x), Set(Constraint(App(le, List(x, two)))))
    val disproveEquation3: Equation = Equation(x, return1(x), Set())
    val disproveEquation4: Equation = Equation(x, y, Set())

    val expansionEquation: Equation = Equation(termFx, App(f, List(four)), Set())
  }

  object TestEquations {

    val monster: Equation = Equation(
      App(u, List(
        App(u, List(
          App(add, List(App(add, List(App(add, List(App(add, List(App(add, List(x, one)), one)), one)), one)), one)),
          x,
          App(returnf, List(x))
        )),
        App(mul, List(x, two)),
        zero
      )),
      App(u, List(
        App(u, List(
          y,
          y,
          App(returnf, List(y))
        )),
        App(mul, List(y, two)),
        zero
      )),
      Set(Constraint(App(eql, List(x, y))))
    )

    val eqT1: Equation = Equation(varInt("y1"), varInt("y2"), Set(Constraint(App(eql, List(varInt("y1"), one))), Constraint(App(eql, List(varInt("y2"), one)))))
    val eqT2: Equation = Equation(App(add, List(App(add, List(one, x)), one)), two, Set())
  }

  object SumUp {
    import Values.*
    import equiv.utils.TheorySymbols.*
    val x: Term = varInt("x")
    val y: Term = varInt("y")
    val i: Term = varInt("i")
    val z: Term = varInt("z")
    val sumUp: FunctionSymbol = funcIntInt("sumup", false)
    val u: FunctionSymbol = funcIntIntIntInt("u", false)
    val returnf: FunctionSymbol = funcIntInt("return", false)
    val sumRecRules: Set[Rule] = Set(
      Rule(App(sumUp, List(x)), App(u, List(x, one, zero)), Set()),
      Rule(App(u, List(x, i, z)), App(u, List(x, App(add, List(i, one)), App(add, List(z, i)))), Set(makeConsBin(i, le, x))),
      Rule(App(u, List(x, i, z)), App(returnf, List(z)), Set(makeConsBin(i, gt, x)))
    )
  }

  /** @return A function symbol of type [Int x Int] => Bool */
  def funcIntIntBool(operator: String, isTheory: Boolean = true, isValue: Boolean = false): FunctionSymbol =
    FunctionSymbol(operator, Typing(List(Sort.Int, Sort.Int), Sort.Bool), isTheory, isValue, Some(Infix(InfixKind.Right, 1)))

  def funcIntInt(operator: String, isTheory: Boolean = true, isValue: Boolean = false): FunctionSymbol =
    FunctionSymbol(operator, Typing(List(Sort.Int), Sort.Int), isTheory, isValue, Some(Infix(InfixKind.Right, 1)))
  
  def funcIntIntInt(operator: String, isTheory: Boolean = true, isValue:Boolean = false): FunctionSymbol =
    FunctionSymbol(operator, Typing(List(Sort.Int, Sort.Int), Sort.Int), isTheory, isValue, Some(Infix(InfixKind.Right, 1)))
  
  def funcIntIntIntInt(operator: String, isTheory: Boolean = true, isValue: Boolean = false): FunctionSymbol =
    FunctionSymbol(operator, Typing(List(Sort.Int, Sort.Int, Sort.Int), Sort.Int), isTheory, isValue, Some(Infix(InfixKind.Right, 1)))

  /** @return A function symbol of type [Bool x Bool] => Bool */
  def funcBoolBoolBool(operator: String, isTheory: Boolean = true, isValue: Boolean = false): FunctionSymbol =
    FunctionSymbol(operator, Typing(List(Sort.Bool, Sort.Bool), Sort.Bool), isTheory, isValue, Some(Infix(InfixKind.Right, 1)))

  /** @return An integer value */
  def valInt(value: Int): Term =
    App(FunctionSymbol.`Int`(value), List())

  /** @return An integer variable */
  def varInt(name: String): Term = Var(name, Sort.Int)

  /** A constraint for an integer variable and an integer value. */
  def consVarIntInt(variableName: String, operatorName: String, value: Int): Constraint =
    makeConsBin(varInt(variableName), funcIntIntBool(operatorName), valInt(value))

  def consVarIntInt2(variableName: String, operatorName: String, value: Int): Constraint =
    makeConsBin(App(funcIntIntInt("-"), List(varInt(variableName), valInt(1))), funcIntIntBool(operatorName), valInt(value - 1))

  def makeConsBin(arg1: Term, fun: FunctionSymbol, arg2: Term): Constraint =
    Constraint(App(fun, List(arg1, arg2)))

}
