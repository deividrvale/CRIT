package equiv.ri.inference_rules

import equiv.ri.ProofState
import equiv.ri.Equation
import equiv.trs.Term.App
import equiv.utils.ListExtension.onNonEmpty

object CONSTRUCTOR extends INFERENCE_RULE {
  val name = "CONSTRUCTOR"

  /** Try to apply CONSTRUCTOR.
   * @param pfSt The current [[ProofState]].
   * @param equationSelector Function that selects an [[Equation]] from a non-empty list of candidate [[Equation]]s for CONSTRUCTOR.
   * @return [[Some]]([[pfSt]]) after application of CONSTRUCTOR, or [[None]] if CONSTRUCTOR was not possible. */
  def tryCONSTRUCTOR(pfSt: ProofState, equationSelector: List[Equation] => Equation): Option[ProofState] = {
    getCONSTRUCTOREquations(pfSt).onNonEmpty(
      eqs => Some( doCONSTRUCTOROnEquation(pfSt, equationSelector(eqs)) )
    )
  }

  /** Apply CONSTRUCTOR to the given equation.
   * @param pfSt The current [[ProofState]].
   * @param equation The [[Equation]] subject to CONSTRUCTOR. Must be in [[pfSt]].
   * @return [[pfSt]] after application of CONSTRUCTOR. */
  def doCONSTRUCTOROnEquation(pfSt: ProofState, equation: Equation): ProofState = {
    equation match {
      case Equation(App(_, args1), App(_, args2), constraints) =>
        val newEquations = args1.zip(args2).map( (s, t) => Equation(s, t, constraints) ).toSet
        pfSt.removeEquation(equation).addEquations(newEquations)
    }
  }

  /** @return A [[List]] of all [[Equation]]s to which CONSTRUCTOR can be applied. May be empty. */
  def getCONSTRUCTOREquations(pfSt: ProofState): List[Equation] = {
    pfSt.equations.filter {
      case Equation(App(f1, _), App(f2, _), _) => f1 == f2 && f1.isConstructor(pfSt.definedSymbols)
      case _ => false
    }.toList
  }
}
