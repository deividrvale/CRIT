package equiv.utils

import equiv.trs.Term.App
import equiv.trs.{Constraint, FunctionSymbol, Sort, Term, Typing}
import equiv.trs.Term.Var

object TermUtils {
  val boolTrue: Term = App(FunctionSymbol("true", Typing(List(), Sort.Bool), isTheory = true),List())
  val boolFalse: Term = App(FunctionSymbol("false", Typing(List(), Sort.Bool), isTheory = true),List())
  def constraintTrue: Constraint = Constraint(boolTrue)
  def constraintFalse: Constraint = Constraint(boolFalse)

  def impl(x: Term, y: Term): App = Term.App(FunctionSymbol("=>", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool), isTheory = true), List(x, y))
  def biImpl(x: Term, y: Term): App = Term.App(FunctionSymbol("<=>", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool), isTheory = true), List(x, y))
  def and(x: Term, y: Term): App = Term.App(FunctionSymbol("and", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool), isTheory = true), List(x, y))
  def not(x: Term): App = Term.App(FunctionSymbol("not", Typing(List(Sort.Bool), Sort.Bool), isTheory = true), List(x))
  def is(x: Term, y: Term): App = Term.App(FunctionSymbol("=", Typing(List(Sort.Any, Sort.Any), Sort.Bool), isTheory = true), List(x, y))

  var lastVarName = "v0"

  def getFreshVarName: String = {
    val newVarName = "v" + (lastVarName.substring(1).toInt + 1).toString
    lastVarName = newVarName
    newVarName
  }

  def getFreshVar(sort: Sort) = Var(getFreshVarName, sort)
}
