package equiv.ri.tactics

import equiv.ri.{Equation, ProofState}
import equiv.trs.Rule
import equiv.ri.Equation.Side
import equiv.trs.Term
import equiv.trs.Term.{Var, App}
import equiv.trs.Constraint


object EXPANSION {
  val name = "EXPANSION"

  /** Look for the first possible equation side that expansion can be performed on and do it.
   * @return The new proofstate after expansion in a `Some`, or `None` if no expansion could be performed */
  def tryExpansion(pfSt: ProofState): Option[ProofState] = {
    tryExpansion2(pfSt).map((newPfSt, maybeRule) => newPfSt.maybeAddRule(maybeRule))
  }

  /** Look for the first possible equation side that expansion can be performed on and do it. Do not yet add the rule.
   * @return The new proofstate after expansion (without rule added), and maybe the rule in a `Some` , or `None` if no expansion could be performed */
  def tryExpansion2(pfSt: ProofState): Option[(ProofState, Option[Rule])] = {
    pfSt.equations.view.flatMap { equation =>
      tryExpansionOnEquation(equation, pfSt.rules, pfSt).map(
        (newEquations, maybeRule) => (pfSt.removeEquation(equation).addEquations(newEquations), maybeRule)
      )
    }.headOption
  }

  /** Look for the first possible place of the given equation (left than right) to to perform an expansion on and do it.
   * @return A generated set of equations and optional rule in a `Some`, or `None` if no expansion could be performed */
  def tryExpansionOnEquation(equation: Equation, rules: Set[Rule], pfSt: ProofState): Option[(Set[Equation], Option[Rule])] = {
    List(Side.Left, Side.Right).view.flatMap( side =>
      tryExpansionOnEquationSide(equation, side, rules, pfSt)
    ).headOption
  }

  /** Try to perform expansion on the given side of the given equation with the given ruleset.
   * @return A generated set of equations and optional rule in a `Some`, or `None` if no expansion could be performed */
  def tryExpansionOnEquationSide(equation: Equation, side: Side, rules: Set[Rule], pfSt: ProofState): Option[(Set[Equation], Option[Rule])] = {
    tryExpansionOnTerm(equation.getSide(side), rules, pfSt)
      .map( terms => {
        val eqs = terms.map( (t, cons) => equation.replaceSide(side, t).addConstraints(cons) )
        val maybeRule = Rule(equation.getSide(side), equation.getOppositeSide(side), equation.constraints).getIfTerminating(rules)
        println(s"$name on $side side of ${equation.toPrintString()} gives ${eqs.map(_.toPrintString())}.\n")
        ( eqs, maybeRule )
      }
    )
  }

  /** Look for the first possible subterm to perform expansion on and do it.
   * @return A generated set of reducts + constraints in a `Some`, or `None` if no expansion could be performed */
  def tryExpansionOnTerm(term: Term, rules: Set[Rule], pfSt: ProofState): Option[Set[(Term, Set[Constraint])]] = {
    term match {
      case Var(_, _) => None
      case t@App(_, args) => tryExpansionOnSubTerm(t, rules, pfSt) match {
        case None => args.view.flatMap(tryExpansionOnTerm(_, rules, pfSt)).headOption
        case Some(x) => Some(x)
      }
    }
  }

  def tryExpansionOnSubTerm(term: Term, rules: Set[Rule], pfSt: ProofState, debug: Boolean = false): Option[Set[(Term, Set[Constraint])]] = {
    if !term.isBasic(pfSt) then { if debug then println(s"${term.toPrintString()} is not basic") ; None } else {
      val applicableRuleSubstitutionPairs = rules.flatMap(rule => term.instanceOf(rule.left).map((rule, _)))
      if applicableRuleSubstitutionPairs.isEmpty then None else {
        Some(applicableRuleSubstitutionPairs.map((rule, sub) => (term.rewriteAtPos(List(), rule, sub), rule.substituteConstraints(sub))))
      }
    }
  }

}
