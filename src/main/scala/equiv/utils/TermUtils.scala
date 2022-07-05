package equiv.utils

import equiv.trs.Term.{App, Position, Var}
import equiv.trs.Term
import equiv.trs.{Constraint, FunctionSymbol, Sort, Term, Typing}

import scala.annotation.tailrec

object TermUtils {
  val conjunctionSymbol: String = "/\\"

  val boolTrue: Term = App(FunctionSymbol("true", Typing(List(), Sort.Bool), isTheory = true),List())
  val boolFalse: Term = App(FunctionSymbol("false", Typing(List(), Sort.Bool), isTheory = true),List())
  def constraintTrue: Constraint = Constraint(boolTrue)
  def constraintFalse: Constraint = Constraint(boolFalse)

  def impl(x: Term, y: Term): App = App(FunctionSymbol("=>", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool), isTheory = true), List(x, y))
  def biImpl(x: Term, y: Term): App = App(FunctionSymbol("<=>", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool), isTheory = true), List(x, y))
  def and(x: Term, y: Term): App = App(FunctionSymbol("and", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool), isTheory = true), List(x, y))
  def not(x: Term): App = App(FunctionSymbol("not", Typing(List(Sort.Bool), Sort.Bool), isTheory = true), List(x))
  def is(x: Term, y: Term): App = App(FunctionSymbol("=", Typing(List(Sort.Any, Sort.Any), Sort.Bool), isTheory = true), List(x, y))
  def notis(x: Term, y: Term): App = not(is(x,y))

  var lastVarName = "v0"

  def getFreshVarName: String = {
    val newVarName = "v" + (lastVarName.substring(1).toInt + 1).toString
    lastVarName = newVarName
    newVarName
  }

  def getFreshVar(sort: Sort): Var = Var(getFreshVarName, sort)

  /** Check if the first [[Position]] is on the path from the second [[Position]] to the root, or below the second position.
   *
   * @example `isOnPathOf([0],       [0,...]) = true`
   * @example `isOnPathOf([0, 1, 0], [0, 1]) = true`
   * @example `isOnPathOf([0, 2],    [0, 1]) = false` */
  @tailrec
  def isOnPathOf(list1: Position, list2: Position): Boolean = {
    (list1, list2) match {
      case (x::xs, y::ys) => x == y && isOnPathOf(xs, ys)
      case (_, List()) => true
      case (List(), _) => true
    }
  }
}
