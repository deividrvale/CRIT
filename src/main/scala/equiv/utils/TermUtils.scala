package equiv.utils

import equiv.ri.Equation
import equiv.trs.Term.{App, Position, Var}
import equiv.trs.*

import scala.annotation.tailrec

object TermUtils {
  val equalityFunctionSymbolName: String = "="
  val RIEqualityFunctionSymbolName: String = "~~"
  val reservedFunctionSymbol: String = RIEqualityFunctionSymbolName ++ RIEqualityFunctionSymbolName

  var lastVarNameInt: Int = 0

  def getFreshVarName: String = {
    val newVarName = "v" + lastVarNameInt.toString
    lastVarNameInt += 1
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

  /** Check if the given string is an [[Int]], i.e. it potentially starts with [[-]] and contains furthermore only digits */
  def isInt(string: String): Boolean = {
    (string.head == '-' || string.head.isDigit) && string.tail.forall(_.isDigit)
  }

  /** If the given [[String]] is an [[Int]], then convert it to a [[FunctionSymbol]] with the corresponding value.
   * @return [[Some]]([[FunctionSymbol]]) if the [[String]] is an [[Int]], otherwise [[None]] */
  def maybeGetValue(string: String): Option[App] = if isInt(string) then Some(TheorySymbols.valInt(string.toInt)) else None

  /** Get the reserved function symbol used in RI (Rewriting Induction) to indicate the equality that is to be proven. */
  def getRIEqualityFunctionSymbol(sort: Sort): FunctionSymbol = {
    FunctionSymbol(reservedFunctionSymbol, Typing(List(sort, sort), Sort.Bool), isTheory = false, isValue = false, None, isTemporary = true)
  }

  /** Get the symbol "=" used for equality in the theory. */
  def getEqualityFunctionSymbol(sort: Sort): FunctionSymbol = {
    FunctionSymbol(equalityFunctionSymbolName, Typing(List(sort, sort), Sort.Bool), isTheory = true)
  }

}
