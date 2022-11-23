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

  /** Check if the given string is an [[Int]], i.e. it potentially starts with [[-]] and contains furthermore 1 or more digits */
  def isInt(string: String): Boolean = {
    string.nonEmpty && ((string.head == '-' && string.length > 1) || string.head.isDigit) && string.tail.forall(_.isDigit)
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

  /** In the given [[Substitution]], replace every occurrence of [[variable]] by [[term]].
   * @param variableToBeReplaced The variable to be replaced in the substitutions
   * @param replacementTerm The term that will replace the variable
   * @param substitution The substitution where replacement will take place
   * @return A new [[Substitution]] with every occurrence of [[variable]] replaced by [[term]]. */
  def replaceVarInSub(variableToBeReplaced: Term.Var, replacementTerm: Term, substitution: Substitution): Substitution = {
    substitution.map((subVariable: Var, subTerm: Term) => (maybeReplaceVarWithVar(subVariable, variableToBeReplaced, replacementTerm), subTerm.substituteAll(variableToBeReplaced, replacementTerm)))
  }

  /** Given a [[replacementTerm]], if this [[Term]] is a [[Var]], look at the [[currentVariable]].
   * If this is the [[variableToBeReplaced]], then return [[replacementTerm]] as a [[Var]],
   * otherwise return [[currentVariable]].
   * @param currentVariable The variable currently looked at that may get replaced.
   * @param variableToBeReplaced The variable that we want to replace.
   * @param replacementTerm The term that we want to replace the variable with.
   * @return [[currentVariable]] if the [[replacementTerm]] is not a [[Var]] or if [[currentVariable]] is not [[variableToBeReplaced]].
   *         Otherwise return [[replacementTerm]] as a [[Var]].
  * @example
   * `maybeReplaceVarWithVar(x, y, f(1)) -> x`
   *
   * `maybeReplaceVarWithVar(x, x, f(1)) -> x`
   *
   * `maybeReplaceVarWithVar(x, x, y) -> y` */
  def maybeReplaceVarWithVar(currentVariable: Var, variableToBeReplaced: Var, replacementTerm: Term): Var = {
    replacementTerm match {
      case v@Var(_, _) => if currentVariable == variableToBeReplaced then v else currentVariable
      case _ => currentVariable
    }
  }

  /** @param variableToBeReplaced Variable that we want to replace.
   * @param replacementTerm Term that will replace the variable.
   * @param termPairs List of pairs of [[Term]]s where we replace occurrences of [[variableToBeReplaced]] with [[replacementTerm]].
   * @return The list of [[Term]] pairs, where every occurrence of [[variableToBeReplaced]] is replaced by [[replacementTerm]]. */
  def replaceVarInTermPairs(variableToBeReplaced: Var, replacementTerm: Term, termPairs: List[(Term, Term)]): List[(Term, Term)] = {
    termPairs.map((t1: Term, t2: Term) => (t1.substituteAll(variableToBeReplaced, replacementTerm), t2.substituteAll(variableToBeReplaced, replacementTerm)))
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
