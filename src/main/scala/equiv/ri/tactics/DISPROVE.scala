package equiv.ri.tactics

import equiv.ri.{ProofState, Equation}
import equiv.trs.Term
import equiv.trs.Term.{Var, App}
import equiv.utils.Z3
import equiv.utils.TermUtils

/** To show that an equation is not an inductive theorem, we must derive ⊥ from a COMPLETE proof state. For this, we use DISPROVE.
 * There are 3 cases to consider, which are defined in the functions [[disproveCase1]], [[disproveCase2]] and [[disproveCase3]]. */
object DISPROVE {
  val name = "DISPROVE"

  /** Try to apply DISPROVE on the first possible equation.
   * @param pfSt The [[ProofState]] to try to apply DISPROVE to.
   * @return [[Some]]`(false)` if DISPROVE could be applied, [[None]] otherwise. */
  def tryDisprove(pfSt: ProofState): Option[Boolean] = {
    if (pfSt.getFlag) {
      pfSt.equations.view.flatMap(equation => tryDisproveOnEquation(equation, pfSt)).headOption
    } else {
      None
    }
  }

  /** Try to apply DISPROVE on the given equation {s ~~ t [ϕ]}.
   * @param equation The [[Equation]] to try to apply DISPROVE on.
   * @param pfSt The [[ProofState]] to try to apply DISPROVE to.
   * @return [[Some]]`(false)` if DISPROVE could be applied, [[None]] otherwise. */
  def tryDisproveOnEquation(equation: Equation, pfSt: ProofState): Option[Boolean] = {
    val s = equation.left; val t = equation.right; val phi = equation.getConstrainsConjunctAsTerm
    if disproveCase1(s, t, phi) || disproveCase2(s, t, phi, pfSt) || disproveCase3(s, t, phi, pfSt) then { println("DISPROVE proofstate.") ; Some(false) } else None
  }

  /** Case 1 where we can disprove an equation:
   * Suppose equation {s ~~ t [ϕ]} with s : ι (i.e. s has sort ι):
   * s, t ∈ Terms(Σ_theory, V), ι is a theory sort, and ϕ ∧ s != t is satisfiable.
   * @return [[true]] if the conditions hold (and DISPROVE is possible). */
  def disproveCase1(s: Term, t: Term, phi: Term): Boolean = {
    s.sort.isTheory // ι is a theory sort,
    && s.isTheory && t.isTheory // s, t ∈ Terms(Σ_theory, V),
    && Z3.satisfiable(TermUtils.and(phi, TermUtils.notis(s, t))) // and ϕ ∧ s != t is satisfiable
  }

  /** Case 2 where we can disprove an equation:
   * Suppose equation {s ~~ t [ϕ]} with s : ι (i.e. s has sort ι):
   * s = f(\vec{s}) and t = g(\vec{t}) with f, g distinct constructors and ϕ satisfiable.
   * @return [[true]] if the conditions hold (and DISPROVE is possible). */
  def disproveCase2(s: Term, t: Term, phi: Term, pfSt: ProofState): Boolean = {
    (s, t) match {
      case (App(f, _), App(g, _)) =>  // s = f(\vec{s}) and t = g(\vec{t})
        f.isConstructor(pfSt.definedSymbols) && g.isConstructor(pfSt.definedSymbols) && f != g // with f, g distinct constructors
        && Z3.satisfiable(phi) // and ϕ satisfiable
      case _ => false
    }
  }

  /** Case 3 where we can disprove an equation:
   * Suppose equation {s ~~ t [ϕ]} with s : ι (i.e. s has sort ι):
   * s ∈ ( V \ Var(ϕ) ), ϕ is satisfiable, at least two different constructors have output sort ι (i.e. same sort as s),
   * and either t is a variable distinct from s or t has the form g(\vec{t}) with g ∈ Cons
   * @return [[true]] if the conditions hold (and DISPROVE is possible). */
  def disproveCase3(s: Term, t: Term, phi: Term, pfSt: ProofState): Boolean = {
    s match { 
      case v_s@Var(_, _) =>
        !phi.vars.contains(v_s) // s ∈ ( V \ Var(ϕ) ),
        && Z3.satisfiable(phi) //
        && pfSt.constructors.count(_.typing.output == s.sort) > 1 &&  // at least two different constructors have output sort ι (i.e. same sort as s),
        // and either 
        (t match {
          case v_t@Var(_,_) => v_s != v_t // t is a variable distinct from s 
          case App(g, _) => g.isConstructor(pfSt.definedSymbols) // or t has the form g(\vec{t}) with g ∈ Cons
        })
      case _ => false 
    }
  }
}