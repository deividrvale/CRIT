package equiv.ri.tactics

import equiv.ri.ProofState
import equiv.ri.Equation
import equiv.trs.Term.App

object CONSTRUCTOR {
  def tryConstructor(pfSt: ProofState): Option[ProofState] = {
    pfSt.equations
      .view
      .flatMap(e1 => tryConstructorOnEquation(e1).map(e2 => { println(s"CONSTRUCTOR on $e1 gives ${e2.map(_.toPrintString())}.") ; pfSt.removeEquation(e1).addEquations(e2) } ))
      .headOption
  }

  def tryConstructorOnEquation(equation: Equation): Option[Set[Equation]] = equation match {
    case Equation(App(f1, args1), App(f2, args2), const) => if f1 == f2 then Some(args1.zip(args2).map((t1, t2) => Equation(t1, t2, const)).toSet) else None
    case _ => None
  }
}
