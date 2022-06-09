package equiv.trs
import equiv.trs.Term.{App, Var}

object Temp {
  /** f : Int => Int */
  val funcF: FunctionSymbol = FunctionSymbol("f", Typing(List(Sort.Int), Sort.Int))
  /** g : Int => Int */
  val funcG: FunctionSymbol = FunctionSymbol("g", Typing(List(Sort.Int), Sort.Int))
  /** x : Int */
  val varX: Term = Var("x", Sort.Int)
  /** - : Int x Int => Int */
  val minus: FunctionSymbol = FunctionSymbol("-", Typing(List(Sort.Int, Sort.Int), Sort.Int, true), Some(Infix(InfixKind.Right, 1)))
  /** 0 : Int */
  val valZero: Term = App(FunctionSymbol("0", Typing(List(), Sort.Int, true)), List())
  /** 1 : Int */
  val valOne: Term = App(FunctionSymbol("1", Typing(List(), Sort.Int, true)), List())
  /** x - 1 : Int */
  val xMinusOne: Term = App(minus, List(varX, valOne))
  /** > : Int x Int => Bool  */
  val funcGT: FunctionSymbol = FunctionSymbol(">", Typing(List(Sort.Int, Sort.Int), Sort.Bool, true), Some(Infix(InfixKind.Right, 1)))
  /** <= : Int x Int => Bool */
  val funcLE: FunctionSymbol = FunctionSymbol("<=", Typing(List(Sort.Int, Sort.Int), Sort.Bool, true), Some(Infix(InfixKind.Right, 1)))
  /** return : Int => Int */
  val funcReturn: FunctionSymbol = FunctionSymbol("return", Typing(List(Sort.Int), Sort.Int))

  /** f( x ) */
  val termFx: Term = App(funcF, List(varX))
  /** g( x ) */
  val termGx: Term = App(funcG, List(varX))
  /** g( f( x ) ) */
  val termGFx: Term = App(funcG, List(App(funcF, List(varX))))
  /** f( x - 1 ) */
  val termFxMinOne: Term = App(funcF, List(xMinusOne))
  /** return( 0 ) */
  val termReturnZero: Term = App(funcReturn, List(valZero))
  /** [ x > 0 ] */
  var consXGTZero: Constraint = Constraint(App(funcGT,List(varX, valZero)))
  /** [ x <= 0 ] */
  var consXLEZero: Constraint = Constraint(App(funcLE, List(varX, valZero)))

  /**  f(x) -> f(x - 1)  [x > 0] */
  val rho1: Rule = Rule(termFx, termFxMinOne, Some(consXGTZero))
  /** f(x) -> return(0) [x <= 0] */
  val rho2: Rule = Rule(termFx, termReturnZero, Some(consXLEZero))

  val system: System = System("", "", "", Signature(Set(funcF, funcReturn)), Set(rho1, rho2))

  /** f( x ) [ true ] */
  val consTermFxTrue: ConstrainedTerm = ConstrainedTerm(termFx, Constraint(Core.boolTrue))
  /** g( x ) [ true ] */
  val consTermGxTrue: ConstrainedTerm = ConstrainedTerm(termGx, Constraint(Core.boolTrue))
  /** g( f( x ) ) [ true ] */
  val consTermGFxTrue: ConstrainedTerm = ConstrainedTerm(termGFx, Constraint(Core.boolTrue))
  /** f( x ) [ x > 0 ] */
  val consTermFxXGTZero: ConstrainedTerm = ConstrainedTerm(termFx, consXGTZero)
}
