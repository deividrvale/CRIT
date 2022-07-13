package equiv.utils

import equiv.trs.Term.{App, Position, Var}
import equiv.trs.*

import scala.annotation.tailrec

object TermUtils {
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

  def isInt(string: String): Boolean = {
    (string.head == '-' || string.head.isDigit) && string.tail.forall(_.isDigit)
  }

  def maybeGetValue(string: String): Option[App] = if isInt(string) then Some(TheorySymbols.valInt(string.toInt)) else None

}
