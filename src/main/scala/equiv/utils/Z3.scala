package equiv.utils

import equiv.ri.Equation
import equiv.trs.parsing.QuasiTerm.{App, InfixChain}
import equiv.trs.*
import equiv.trs.parsing.{QuasiSystem, QuasiTerm, QuasiTheory, TRSParser, Z3Parser}
import equiv.trs.{FunctionSymbol, Sort, Term, Typing, parsing}

import java.io.{File, PrintWriter}
import sys.process.*

enum SolverResult:
  case Satisfiable, Unsatisfiable, Undetermined

object Z3 {
  /** @return [[Some]]([[true]]) if the implication is satisfiable.
   *         [[Some]]([[false]]) if the implication is not satisfiable.
   *         [[None]] if it is unknown. */
  def implies(term1: Term, term2: Term): Option[Boolean] = {
    val formula = TheorySymbols.notX(TheorySymbols.implXY(term1, term2))
    val result = solve(formula)
    result match {
      case SolverResult.Unsatisfiable => Some(true)
      case SolverResult.Satisfiable => Some(false)
      case SolverResult.Undetermined => None
    }
  }

  /** Check if a term is satisfiable.
   * @return [[Some]]([[true]]) if the term is satisfiable.
   * [[Some]]([[false]]) if the term is unsatisfiable.
   * [[None]] if it is unknown. */
  def satisfiable(term: Term): Option[Boolean] = {
    solve(term) match {
      case SolverResult.Satisfiable => Some(true)
      case SolverResult.Unsatisfiable => Some(false)
      case SolverResult.Undetermined => None
    }
  }

  def simplifyEquation(equation: Equation): Equation = {
    Equation(simplifyTerm(equation.left), simplifyTerm(equation.right), Constraint(simplifyTerm(equation.getConstrainsConjunctAsTerm)).split())
  }

  /** TODO */
  def simplifyTerm(formula: Term): Term = {
    val inputFile: File = File.createTempFile("input", ".smt2")
    val q =
      s"""|${formula.vars.map { v => s"(declare-const $v ${v.sort})" }.mkString("\n")}
          |${formula.functionSymbols.map(f => if !f.isTheory then s"(declare-fun $f ${f.typing.input.mkString("(", " ", ")")} ${f.typing.output})" else "").mkString(sep = "\n")}
          |(simplify ${formula.toStringApplicative})
          |""".stripMargin
    new PrintWriter(inputFile) {
      write(q)
      close()
    }
    val out: String = Seq("z3", "-smt2", inputFile.getAbsolutePath).!!.linesIterator.next()

    new Z3Parser(out, formula.functionSymbols.map(f => (f.name, f)).toMap, formula.vars.map(v => (v.name, v)).toMap).parseTerm() match {
      case Left(t: Term) => t
      case Right(s: String) => println(s); formula
    }
  }

  def solve(formula: Term): SolverResult = {
    val output: Iterator[String] = query(
      s"""${formula.vars.map { v => s"(declare-const $v ${v.sort})" }.mkString("\n")}
         |${formula.functionSymbols.map(f => if !f.isTheory then s"(declare-fun $f ${f.typing.input.mkString("(", " ", ")")} ${f.typing.output})" else "").mkString(sep = "\n")}
         |
         |(assert
         |   ${formula.toStringApplicative}
         |)
         |
         |(check-sat)"""
    )

    output.next() match {
      case "sat" => SolverResult.Satisfiable
      case "unsat" => SolverResult.Unsatisfiable
      case _ => SolverResult.Undetermined
    }
  }

  private def query(query: String, produceModels: Boolean = false, logic: String = "QF_LIA"): Iterator[String] = {
    val inputFile: File = File.createTempFile("input", ".smt2")

    new PrintWriter(inputFile) {
      write(
        s"""(set-option :produce-models $produceModels)
           |(set-logic $logic)
           |
           |$query
           |""".stripMargin)
      close()
    }

//    For debugging:
//    println(inputFile.getAbsolutePath)
//    Thread.sleep(100000)

    try {
      Seq("z3", "-smt2", inputFile.getAbsolutePath).!!.linesIterator
    } catch {
      t => println(s"Z3 error: ${t}") ; Iterator("undetermined")
    }
  }
}
