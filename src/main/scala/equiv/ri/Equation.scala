package equiv.ri

import dotty.tools.dotc.ast.untpd.Mod.Infix
import equiv.ri.Equation.Side
import equiv.trs.Term.{App, Position, Substitution}
import equiv.trs.{ConstrainedObject, ConstrainedTerm, Constraint, FunctionSymbol, Infix, Rule, Term, Typing}
import equiv.utils.TermUtils

import scala.annotation.targetName
import scala.util.Random
import equiv.trs.Term.Var

object Equation {
  enum Side:
    case Left, Right

  def fromRule(rule: Rule): Equation = Equation(rule.left, rule.right, rule.constraints)
}

case class Equation(left: Term, right : Term, var constraints : Set[Constraint]) extends ConstrainedObject(constraints) {

  val vars: Set[Var] = left.vars ++ right.vars ++ constraintVars

  val functionSymbols: Set[FunctionSymbol] = left.functionSymbols ++ right.functionSymbols ++ constraints.flatMap(_.term.functionSymbols)

  def getSide(side: Side): Term = side match {
    case Side.Left => left
    case Side.Right => right
  }

  def getOppositeSide(side: Side): Term = side match {
    case Side.Left => right
    case Side.Right => left
  }

  def replaceSide(side: Side, term: Term): Equation = side match {
    case Side.Left => this.copy(left = term)
    case Side.Right => this.copy(right = term)
  }

  def rewriteSideAtPos(side: Side, position: Position, rule: Rule, substitution: Substitution): Equation = {
    this.replaceSide(side, this.getSide(side).rewriteAtPos(position, rule, substitution))
  }

  def replaceAllConstraints(newConstraints: Set[Constraint]): Equation =
    this.copy(constraints = newConstraints)

  def addConstraint(newConstraint: Constraint): Equation =
    this.copy(constraints = constraints + newConstraint)

  def addConstraint(newConstraint: Term): Equation =
    this.copy(constraints = constraints + Constraint(newConstraint))

  def addConstraints(newConstraints: Set[Constraint]): Equation =
    this.copy(constraints = constraints ++ newConstraints)

  def removeConstraint(constraint: Constraint): Equation =
    this.copy(constraints = constraints - constraint)

  def removeConstraints(constraintsToRemove: Set[Constraint]): Equation =
    this.copy(constraints = constraints -- constraintsToRemove)

  def withConstraint(newConstraints: Set[Constraint]): Equation =
    this.copy(constraints = newConstraints)

  def simplifyCons(): Equation = this.copy(constraints = super.simplify())

  /** @param side The left side of the rule.
   * @return A rewrite rule of the equation, where the orientation is determined by [[side]]. */
  def getAsRule(side: Side = Side.Left): Rule = {
    Rule(this.getSide(side), this.getOppositeSide(side), constraints)
  }

  /** Check if the current equation is an instance of the given equation.
   * @param equation The equation to check for being more general.
   * @return [[Some]]([[Substitution]]) if [[this]] is an instance of [[equation]], [[None]] otherwise. */
  def instanceOf(equation: Equation): Option[Substitution] =
    this.toConstrainedTerm.instanceOf(equation.toConstrainedTerm)

  /** Get the equation represented as a constrained term, where the equality symbol (~~) is seen as a fresh function symbol. */
  def toConstrainedTerm: ConstrainedTerm =
    ConstrainedTerm(App(TermUtils.getRIEqualityFunctionSymbol(this.left.sort), List(left, right)), constraints)

  // *************************************************** TACTICS *************************************************** //

  /** Transform the equation to a constrained term, where ~~ is seen as a fresh symbol. Then rewrite the constrained term using the given  */
  def SIMPLIFICATION(rule: Rule, side: Side, position: Term.Position, substitution: Substitution): Equation = {
    ConstrainedTerm(App(getFreshFunctionSymbol(rule), List(this.left, this.right)), this.constraints)
      .rewriteAtPos(List(side match { case Side.Left => 0; case Side.Right => 1 }) ++ position, rule, substitution)
    match {
      case ConstrainedTerm(App(_, args), cons) => Equation(args.head, args(1), cons)
    }
  }

  /** @return A FunctionSymbol that does not occur in the equation or given rule,
   *         with Typing such that it is the parent of the left and right side of the equation */
  private def getFreshFunctionSymbol(rule: Rule): FunctionSymbol = {
    val funcNames = left.functionSymbolNames ++ right.functionSymbolNames ++ constraints.flatMap(_.term.functionSymbolNames) ++ rule.right.functionSymbolNames
    var freshName = TermUtils.RIEqualityFunctionSymbolName
    while(funcNames.contains(freshName)) {
      freshName = new Random().nextString(4)
    }
    FunctionSymbol(freshName, Typing(List(left.sort, right.sort), left.sort), isTheory = false, isValue = false, None)
  }

  override def toString: String = toPrintString(false)

  override def toPrintString(colours: Boolean = true): String = s"${left.toPrintString(colours)} ${TermUtils.RIEqualityFunctionSymbolName} ${right.toPrintString(colours)}${if constraints.nonEmpty then " " else ""}${super.toPrintString(colours)}"
}
