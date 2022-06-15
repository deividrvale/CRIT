package equiv.utils

import equiv.trs.Term.App
import equiv.trs.{Constraint, FunctionSymbol, Sort, Term, Typing}

object TermUtils {
  val boolTrue: Term = App(FunctionSymbol("true", Typing(List(), Sort.Bool, true)),List())
  val boolFalse: Term = App(FunctionSymbol("false", Typing(List(), Sort.Bool, true)),List())
  def constraintTrue: Constraint = Constraint(boolTrue)
  def constraintFalse: Constraint = Constraint(boolFalse)

  def impl(x: Term, y: Term): App = Term.App(FunctionSymbol("=>", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool)), List(x, y))
  def and(x: Term, y: Term): App = Term.App(FunctionSymbol("and", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool)), List(x, y))
}
