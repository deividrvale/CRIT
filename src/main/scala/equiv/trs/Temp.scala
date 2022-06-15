package equiv.trs
import equiv.trs.Term.{App, Var}
import equiv.utils.TermUtils.constraintTrue

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
  /** [ x > 0 ] */
  var consXGTZero: Constraint = Constraint(App(funcGT,List(varX, valZero)))
  /** [ x <= 0 ] */
  var consXLEZero: Constraint = Constraint(App(funcLE, List(varX, valZero)))

  /**  f(x) -> f(x - 1)  [x > 0] */
  val rho1: Rule = Rule(termFx, termFxMinOne, (consXGTZero))
  /** f(x) -> return(0) [x <= 0] */
  val rho2: Rule = Rule(termFx, termReturnZero, (consXLEZero))

  val system: System = System("", "", "", Signature(Set(funcF, funcReturn)), Set(rho1, rho2))

  /** f( x ) [ true ] */
  val consTermFxTrue: ConstrainedTerm = ConstrainedTerm(termFx, constraintTrue)
  /** g( x ) [ true ] */
  val consTermGxTrue: ConstrainedTerm = ConstrainedTerm(termGx, constraintTrue)
  /** g( f( x ) ) [ true ] */
  val consTermGFxTrue: ConstrainedTerm = ConstrainedTerm(termGFx, constraintTrue)
  /** f( x ) [ x > 0 ] */
  val consTermFxXGTZero: ConstrainedTerm = ConstrainedTerm(termFx, consXGTZero)
}
