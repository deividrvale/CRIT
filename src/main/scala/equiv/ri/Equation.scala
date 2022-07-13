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
}

case class Equation(left: Term, right : Term, var constraints : Set[Constraint]) extends ConstrainedObject(constraints) {

  val vars: Set[Var] = left.vars ++ right.vars ++ constraints.flatMap(_.term.vars)

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

  def replaceAllConstraints(newConstraints: Set[Constraint]): Equation =
    this.copy(constraints = newConstraints)

  def addConstraint(newConstraint: Constraint): Equation =
    this.copy(constraints = constraints + newConstraint)

  def addConstraint(newConstraint: Term): Equation =
    this.copy(constraints = constraints + Constraint(newConstraint))

  def addConstraints(newConstraints: Set[Constraint]): Equation =
    this.copy(constraints = constraints ++ newConstraints)

  def withConstraint(newConstraints: Set[Constraint]): Equation =
    this.copy(constraints = newConstraints)

  def simplifyCons(): Equation = this.copy(constraints = super.simplify())

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
    var freshName = "~~"
    while(funcNames.contains(freshName)) {
      freshName = new Random().nextString(4)
    }
    FunctionSymbol(freshName, Typing(List(left.sort, right.sort), left.sort), isTheory = false, isValue = false, Some(equiv.trs.Infix(equiv.trs.InfixKind.Left, 0)))
  }

  override def toString: String = toPrintString(false)

  override def toPrintString(colours: Boolean = true): String = s"${left.toPrintString(colours)} ~~ ${right.toPrintString(colours)} ${super.toPrintString(colours)}"
}
