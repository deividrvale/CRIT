package equiv.trs
import equiv.trs.Term.{App, Var}
import equiv.utils.TermUtils.constraintTrue
import equiv.ri.Equation
import equiv.trs.System

object Temp {
  /** f : Int => Int */
  val funcF: FunctionSymbol = FunctionSymbol("f", Typing(List(Sort.Int), Sort.Int))
  /** g : Int => Int */
  val funcG: FunctionSymbol = FunctionSymbol("g", Typing(List(Sort.Int), Sort.Int))
  /** x : Int */
  val varX: Term = Var("x", Sort.Int)
  /** y : Int */
  val varY: Term = Var("y", Sort.Int)
  /** - : Int x Int => Int */
  val minus: FunctionSymbol = FunctionSymbol("-", Typing(List(Sort.Int, Sort.Int), Sort.Int), isTheory = true, isValue = false, Some(Infix(InfixKind.Right, 1)))
  /** 0 : Int */
  val valZero: Term = App(FunctionSymbol("0", Typing(List(), Sort.Int), isTheory = true, isValue = true), List())
  /** 1 : Int */
  val valOne: Term = App(FunctionSymbol("1", Typing(List(), Sort.Int), isTheory = true, isValue = true), List())
  /** x - 1 : Int */
  val xMinusOne: Term = App(minus, List(varX, valOne))
  /** = : Int x Int => Bool  */
  val funcEq: FunctionSymbol = FunctionSymbol("=", Typing(List(Sort.Int, Sort.Int), Sort.Bool), isTheory = true, isValue = false, Some(Infix(InfixKind.Right, 1)))
  /** > : Int x Int => Bool  */
  val funcGT: FunctionSymbol = FunctionSymbol(">", Typing(List(Sort.Int, Sort.Int), Sort.Bool), isTheory = true, isValue = false, Some(Infix(InfixKind.Right, 1)))
  val funcGE: FunctionSymbol = FunctionSymbol(">=", Typing(List(Sort.Int, Sort.Int), Sort.Bool), isTheory = true, isValue = false, Some(Infix(InfixKind.Right, 1)))
  /** <= : Int x Int => Bool */
  val funcLE: FunctionSymbol = FunctionSymbol("<=", Typing(List(Sort.Int, Sort.Int), Sort.Bool), isTheory = true, isValue = false, Some(Infix(InfixKind.Right, 1)))
  /** < : Int x Int => Bool */
  val funcLT: FunctionSymbol = FunctionSymbol("<", Typing(List(Sort.Int, Sort.Int), Sort.Bool), isTheory = true, isValue = false, Some(Infix(InfixKind.Right, 1)))
  /** return : Int => Int */
  val funcReturn: FunctionSymbol = FunctionSymbol("return", Typing(List(Sort.Int), Sort.Int), isTheory = false, isValue = false)

  /** f( x ) */
  val termFx: Term = App(funcF, List(varX))
  /** f( y ) */
  val termFy: Term = App(funcF, List(varY))
  /** g( x ) */
  val termGx: Term = App(funcG, List(varX))
  /** g( y ) */
  val termGy: Term = App(funcG, List(varY))
  /** g( f( x ) ) */
  val termGFx: Term = App(funcG, List(App(funcF, List(varX))))
  /** g( f( y ) ) */
  val termGFy: Term = App(funcG, List(App(funcF, List(varY))))
  /** f( x - 1 ) */
  val termFxMinOne: Term = App(funcF, List(xMinusOne))
  /** return( 0 ) */
  val termReturnZero: Term = App(funcReturn, List(valZero))
  /** return( x ) */
  val termReturnX: Term = App(funcReturn, List(varX))
  /** [ x > 0 ] */
  var consXGTZero: Constraint = Constraint(App(funcGT,List(varX, valZero)))
  var consXGEZero: Constraint = Constraint(App(funcGE,List(varX, valZero)))
  /** [ x <= 0 ] */
  val consXLEZero: Constraint = Constraint(App(funcLE, List(varX, valZero)))
  /** [ x < 0 ] */
  val consXLTZero: Constraint = Constraint(App(funcLT, List(varX, valZero)))
  /** [ x = 0 ] */
  val consXEqZero: Constraint = Constraint(App(funcEq, List(varX, valZero)))

  /**  f(x) -> f(x - 1)  [x > 0] */
  val rho1: Rule = Rule(termFx, termFxMinOne, Set(consXGTZero))
  /** f(x) -> return(0) [x <= 0] */
  val rho2: Rule = Rule(termFx, termReturnZero, Set(consXLEZero))

  val system: System = System("", "", "", Signature(Set(funcF, funcReturn)), Set(rho1, rho2))

  /** f( x ) [ true ] */
  val consTermFxTrue: ConstrainedTerm = ConstrainedTerm(termFx, Set())
  /** g( x ) [ true ] */
  val consTermGxTrue: ConstrainedTerm = ConstrainedTerm(termGx, Set())
  /** g( f( x ) ) [ true ] */
  val consTermGFxTrue: ConstrainedTerm = ConstrainedTerm(termGFx, Set())
  /** f( x ) [ x > 0 ] */
  val consTermFxXGTZero: ConstrainedTerm = ConstrainedTerm(termFx, Set(consXGTZero))

  object Theory {
    val add: FunctionSymbol = funcIntIntInt("+")
    val mult: FunctionSymbol = funcIntIntInt("*")
    val div: FunctionSymbol = funcIntIntInt("/")
    val min: FunctionSymbol = funcIntIntInt("-")
    val le: FunctionSymbol = funcIntIntBool("<=")
    val ge: FunctionSymbol = funcIntIntBool(">=")
    val lt: FunctionSymbol = funcIntIntBool("<")
    val gt: FunctionSymbol = funcIntIntBool(">")
    val eql: FunctionSymbol = funcIntIntBool("=")
    val zero: Term = valInt(0)
    val one: Term = valInt(1)
    val two: Term = valInt(2)
    val four: Term = valInt(4)
  }

  object SumUp {
    import Theory.*
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

    val equation: Equation = Equation(
      App(u, List(
        App(u, List(
          x,
          x,
          App(returnf, List(x))
        )),
        App(mult, List(x, two)),
        zero
      )),
      App(u, List(
        App(u, List(
          y,
          y,
          App(returnf, List(y))
        )),
        App(mult, List(y, two)),
        zero
      )),
      Set(Constraint(App(eql, List(x, y))))
    )

    val equation2: Equation = Equation(termGy, termFx, Set(
      Constraint(App(lt, List(x, y))),
      Constraint(App(Theory.eql, List(App(mult, List(x, two)), four))),
      Constraint(App(gt, List(two, App(div, List(App(add, List(y, y)), two))))))
    )

    val eqT1: Equation = Equation(varInt("y1"), varInt("y2"), Set(Constraint(App(eql, List(varInt("y1"), one))), Constraint(App(eql, List(varInt("y2"), one)))))
    val eqT2: Equation = Equation(App(add, List(App(add, List(one, x)), one)), two, Set())
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
