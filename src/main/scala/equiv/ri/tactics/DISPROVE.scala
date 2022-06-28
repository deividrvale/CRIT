package equiv.ri.tactics

import equiv.ri.{ProofState, Equation}
import equiv.trs.Term
import equiv.trs.Term.{Var, App}
import equiv.utils.Z3
import equiv.utils.TermUtils

object DISPROVE {
  /** @return `Some(false)` if the current proofstate can be disproven, `None` otherwise. */
  def tryDisprove(pfSt: ProofState): Option[Boolean] = pfSt.getFlag match {
    case false => None
    case true => pfSt.equations.view.flatMap( equation =>
      val s = equation.left; val t = equation.right; val phi = equation.getConstrainsConjunctAsTerm 
      if disProveCase1(s, t, phi) || disProveCase2(s, t, phi, pfSt) || disProveCase3(s, t, phi, pfSt) then Some(false) else None
      ).headOption
  }

  def disProveCase1(s: Term, t: Term, phi: Term): Boolean = {
    s.sort.isTheory // i is a theory sort,
    && s.isTheory && t.isTheory // s, t \in Terms(Sigma_theory, V),
    && Z3.satisfiable(TermUtils.and(phi, TermUtils.notis(s, t))) // and phi /\ s != t is satisfiable
  }

  def disProveCase2(s: Term, t: Term, phi: Term, pfSt: ProofState): Boolean = {
    (s, t) match { 
      case (App(f, _), App(g, _)) =>  // s = f(\vec{s}) and t = g(\vec{t})
        pfSt.constructors.contains(f) && pfSt.constructors.contains(g) && f != g // with f, g distinct constructors
        && Z3.satisfiable(phi) // and phi satisfiable
      case _ => false 
    }
  }
  
  def disProveCase3(s: Term, t: Term, phi: Term, pfSt: ProofState): Boolean = {
    s match { 
      case (v_s@Var(_, _)) => 
        !phi.vars.contains(v_s) // s \in ( V \ Var(phi) ),
        && Z3.satisfiable(phi) // phi is satisfiable,
        && pfSt.constructors.filter(_.typing.output == s.sort).size > 1 &&  // at least two different constructors have output sort i (same sort as s),
        // and either 
        (t match {
          case v_t@Var(_,_) => v_s != v_t // t is a variable distinct from s 
          case App(g, _) => pfSt.constructors.contains(g) // or t has the form g(\vec{t}) with g \in Cons
        })
      case _ => false 
    }
  }
}