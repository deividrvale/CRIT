package equiv.utils

import com.sun.org.apache.xpath.internal.operations.Variable
import equiv.ri.Equation
import equiv.trs.Term.{App, Position, Substitution, Var}
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

  def replaceVarInSub(variable: Term.Var, term: Term, substitution: Substitution): Substitution = {
    substitution.map((variable2: Var, term2: Term) => (variable2, term2.substituteAll(variable, term)))
  }

  def replaceVarInTermPairs(variable: Var, term: Term, equations: List[(Term, Term)]): List[(Term, Term)] = {
    equations.map((t1: Term, t2: Term) => (t1.substituteAll(variable, term), t2.substituteAll(variable, term)))
  }

  /** @return If the given substitution is a map from variables to variables,
   *         return [[Some]]([[Substitution]]) where every element is reversed as long as there are no conflicting variable maps.
   *         If not, then return [[None]].
   * @example `maybeReverseMap( { x -> y, z -> y } )` gives [[None]], since the reverse maps `y` to both `x` and `z`.
   *
   * `maybeReverseMap( { x -> z } )` gives [[Some]]`({ z -> x })`.
   *
   * `maybeReverseMap( { x -> 1 } )` gives [[None]] */
  def maybeReverseMap(substitution: Substitution): Option[Substitution] = {
    var reverseSubstitution: Substitution = Map()
    substitution.foreach((v, t) => t match { case v2@Var(_, _) => reverseSubstitution = reverseSubstitution.updated(v2, v) ; case _ => return None })
    if reverseSubstitution.size == substitution.size then
      Some(reverseSubstitution)
    else None
  }
}
