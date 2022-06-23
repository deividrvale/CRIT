package equiv.ri.tactics

import equiv.ri.{Equation, ProofState}
import equiv.trs.Rule
import equiv.ri.Equation.Side
import equiv.trs.Term
import equiv.trs.Term.{Var, App}
import equiv.trs.Constraint


object EXPANSION {

  def tryExpansion(pfSt: ProofState): Option[(Equation, Set[Equation], Option[Rule])] = {
    pfSt.equations.view.flatMap { equation =>
      tryExpansionOnEquation(equation, pfSt.rules).map(
        (equations, maybeRule) => (equation, equations, maybeRule)
      )
    }.headOption
  }

  def tryExpansionOnEquation(equation: Equation, rules: Set[Rule]): Option[(Set[Equation], Option[Rule])] = {
    List(Side.Left, Side.Right).view.flatMap{ side =>
      tryExpansionOnEquationSide(equation, side, rules)
    }.headOption
  }

  def tryExpansionOnEquationSide(equation: Equation, side: Side, rules: Set[Rule]): Option[(Set[Equation], Option[Rule])] = {
    tryExpansionOnTerm(equation.getSide(side), rules)
      .map( terms =>
        println(s"EXPANSION on ${equation.getSide(side).toPrintString()}.")
        ( terms.map( (t, cons) => equation.replaceSide(side, t).addConstraints(cons) ), 
          Rule(equation.getSide(side), equation.getOppositeSide(side), equation.constraints).getIfTerminating(rules) ) )
  }

  def tryExpansionOnTerm(term: Term, rules: Set[Rule]): Option[(Set[(Term, Set[Constraint])])] = {
    term match {
      case Var(_, _) => None
      case t@App(f, args) => tryExpansionOnSubTerm(t, rules) match {
        case None => args.view.flatMap(tryExpansionOnTerm(_, rules)).headOption
        case Some(x) => Some(x)
      }
    }
  }

  def tryExpansionOnSubTerm(term: Term, rules: Set[Rule]): Option[Set[(Term, Set[Constraint])]] = {
    val applicableRuleSubstitutionPairs = rules.map(rule => term.instanceOf(rule.left).map((rule, _)) ).flatten
    if applicableRuleSubstitutionPairs.isEmpty then None 
    else Some(applicableRuleSubstitutionPairs.map((rule, sub) => (term.rewriteAtPos(List(), rule, sub), rule.substituteConstraints(sub))))
  }

}
