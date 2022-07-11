package equiv.ri.inference_rules

import equiv.ri.ProofState
import equiv.ri.Equation
import equiv.trs.Term.App

object CONSTRUCTOR {
  val name = "CONSTRUCTOR"
  /** Step-wise try to apply CONSTRUCTOR to every equation in the proofstate.
   * @return [[Some]](proofstate) after the first successful CONSTRUCTOR, or [[None]] if CONSTRUCTOR could not be applied. */
  def tryConstructor(pfSt: ProofState): Option[ProofState] = {
    pfSt.equations.view.flatMap(e1 => tryConstructorOnEquation(e1, pfSt)).headOption
  }

  /** Check if we can apply CONSTRUCTOR to the given equation, i.e. if the equation is of the form `f(s_1,...,s_n) ~~ f(t_1,...,t_n) [phi]`, where `f` is a constructor.
   * @param equation The equation that is checked for application.
   * @param pfSt The proofstate containing the equation.
   * @param succeedDebug Whether to print on a successful application.
   * @param failDebug Whether to print on a failed application.
   * @return [[Some]](proofstate), where the proofstate does not contain the original equation, but equations `s_i ~~ t_i`, or [[None]] if CONSTRUCTOR could not be applied to the equation. */
  def tryConstructorOnEquation(equation: Equation, pfSt: ProofState, succeedDebug: Boolean = true, failDebug: Boolean = false): Option[ProofState] = {
    equation match {
      case Equation(App(f1, args1), App(f2, args2), const) =>
        if f1 == f2 && f1.isConstructor(pfSt.definedSymbols) then
          val equations = args1.zip(args2).map((t1, t2) => Equation(t1, t2, const)).toSet
          if (succeedDebug) { println(s"$name on ${equation.toPrintString()} gives ${equations.map(_.toPrintString())}.") }
          return Some(pfSt.removeEquation(equation).addEquations( equations ))
    }
    if (failDebug) { println(s"$name failed.") }
    None
  }
}
