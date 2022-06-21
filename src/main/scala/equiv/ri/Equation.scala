package equiv.ri

import dotty.tools.dotc.ast.untpd.Mod.Infix
import equiv.ri.Equation.Side
import equiv.trs.Term.{App, Position, Substitution}
import equiv.trs.{ConstrainedObject, ConstrainedTerm, Constraint, FunctionSymbol, Infix, Rule, Term, Typing}
import equiv.utils.TermUtils

import scala.annotation.targetName
import scala.util.Random

object Equation {
  enum Side:
    case Left, Right
}

case class Equation(left: Term, right : Term, var constraints : Set[Constraint]) extends ConstrainedObject(constraints) {
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

  def addConstraint(newConstraint: Constraint): Equation =
    this.copy(constraints = constraints + newConstraint)

  def addConstraint(newConstraint: Term): Equation =
    this.copy(constraints = constraints + Constraint(newConstraint))

  def addConstraints(newConstraints: Set[Constraint]): Equation =
    this.copy(constraints = constraints ++ newConstraints)

//  def addConstraints(newConstraints: Set[Term]): Equation =
//    this.copy(constraints = constraints ++ newConstraints.map(Constraint.apply))

  def withConstraint(newConstraints: Set[Constraint]): Equation =
    this.copy(constraints = newConstraints)

  /** TODO Get the Expd set of an equation */
  def getExpd(side: Side): Set[Equation] = ???

  /** TODO Get the pairwise set of equalities for the constructor arguments in the equation */
  def getConstructorArguments: Set[Equation] = ???

  /** TODO Check if the rule generated by this equation is terminating. If so, return it in a set, otherwise return the empty set. */
  def maybeHypothesis(side: Side, rules: Set[Rule]): Set[Rule] = {
    val rule = Rule(this.getSide(side), this.getOppositeSide(side), this.constraints)
    if rule.isTerminating(rules) then Set(rule) else Set()
  }

  def simplifyCons(): Equation = this.copy(constraints = super.simplify())

  // *************************************************** TACTICS *************************************************** //

  /** Transform the equation to a constrained term, where ~~ is seen as a fresh symbol. Then rewrite the constrained term using the given  */
  def SIMPLIFICATION(rule: Rule, side: Side, position: Term.Position, substitution: Substitution): Equation = {
    ConstrainedTerm(App(getFreshFunctionSymbol(rule), List(this.left, this.right)), this.constraints)
      .rewriteAtPos(List(side match { case Side.Left => 0; case Side.Right => 1 }) ++ position, rule.right, substitution)
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
    FunctionSymbol(freshName, Typing(List(left.sort, right.sort), left.sort), Some(equiv.trs.Infix(equiv.trs.InfixKind.Left, 0)))
  }

  override def toString: String = s"$left ~~ $right ${super.toString}"

  override def toPrintString(colours: Boolean = true): String = s"${left.toPrintString(colours)} ~~ ${right.toPrintString(colours)} ${super.toPrintString(colours)}"
}
