package equiv.utils

import equiv.trs.Term.App
import equiv.trs.*

object TheorySymbols {
  // ==================== MAIN THEORY SYMBOLS =====================================
  val t: FunctionSymbol      = funcBool("true")
  val f: FunctionSymbol      = funcBool("false")
  val not: FunctionSymbol    = funcBoolBool("not")
  val and: FunctionSymbol    = funcBoolBoolBool("and")
  val or: FunctionSymbol     = funcBoolBoolBool("or")
  val impl: FunctionSymbol   = funcBoolBoolBool("=>")
  val biImpl: FunctionSymbol = funcBoolBoolBool("<=>")

  // ===================== INT THEORY SYMBOLS =====================================
  val add: FunctionSymbol    = funcIntIntInt("+")
  val mul: FunctionSymbol    = funcIntIntInt("*")
  val div: FunctionSymbol    = funcIntIntInt("/")
  val min: FunctionSymbol    = funcIntIntInt("-")
  val le: FunctionSymbol     = funcIntIntBool("<=")
  val ge: FunctionSymbol     = funcIntIntBool(">=")
  val lt: FunctionSymbol     = funcIntIntBool("<")
  val gt: FunctionSymbol     = funcIntIntBool(">")
  val eql: FunctionSymbol    = FunctionSymbol(TermUtils.equalityFunctionSymbolName, Typing(List(Sort.Any, Sort.Any), Sort.Bool), isTheory = true, infix = defaultInfix)

  /** Map of all [[FunctionSymbol]]s in the theory as values and their names ([[String]]) as keys. */
  val theorySymbols: Map[String, FunctionSymbol] = List(t, f, not, and, or, impl, biImpl, add, mul, div, min, le, ge, lt, gt, eql).map(f => (f.name, f)).toMap

  def constraintFalse: Constraint = Constraint(App(t, List()))
  def constraintTrue: Constraint = Constraint(App(f, List()))

  def boolTrue: App = App(t, List())
  def boolFalse: App = App(f, List())
  def notX(x: Term): App = App(not, List(x))
  def implXY(x: Term, y: Term): App = App(impl, List(x, y))
  def biImplXY(x: Term, y: Term): App = App(biImpl, List(x, y))
  def eqXY(x: Term, y: Term): App = App(eql, List(x, y))
  def notEqXY(x: Term, y: Term): App = notX(eqXY(x,y))
  def andXY(x: Term, y: Term): App = App(and, List(x, y))
  def orXY(x: Term, y: Term): App = App(or, List(x, y))

  // ================== HELPER FUNCTIONS =============================
  def defaultInfix: Option[Infix] = Some(Infix(InfixKind.Left, 1)) // For some reason this must be a def, not a val! Otherwise it becomes null
  def funcBool(name: String): FunctionSymbol =
    FunctionSymbol(name, Typing(List(), Sort.Bool), isTheory = true, isValue = true)
  def funcBoolBool(name: String): FunctionSymbol =
    FunctionSymbol(name, Typing(List(Sort.Bool), Sort.Bool), isTheory = true)
  def funcBoolBoolBool(name: String, theory: Boolean = true): FunctionSymbol =
    FunctionSymbol(name, Typing(List(Sort.Bool, Sort.Bool), Sort.Bool), isTheory = theory, infix = defaultInfix)
  def funcIntIntBool(name: String, theory: Boolean = true): FunctionSymbol =
    FunctionSymbol(name, Typing(List(Sort.Int, Sort.Int), Sort.Bool), isTheory = theory, infix = defaultInfix)
  def funcIntIntInt(name: String, theory: Boolean = true): FunctionSymbol =
    FunctionSymbol(name, Typing(List(Sort.Int, Sort.Int), Sort.Int), isTheory = theory)
  def valInt(value: Int): App = App(FunctionSymbol.`Int`(value), List())
}
