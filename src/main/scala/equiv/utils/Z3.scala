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
  def supportedSorts : List[Sort] = List(Sort.Int, Sort.Bool)

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
    val newLeft = Z3_simplify_term(equation.left)
    val newRight = Z3_simplify_term(equation.right)
    val simplifiedConstraints0 = equation.constraints
    val simplifiedConstraints1 = Z3_solver_subsumption_constraints(simplifiedConstraints0)
    val simplifiedConstraints2: Set[Constraint] = simplifiedConstraints1.map(c => Constraint(Z3_simplify_term(c.term)))

    Equation(newLeft, newRight, simplifiedConstraints2)
  }

  /** TODO */
  def Z3_simplify_term(formula: Term): Term = {
    val inputFile: File = File.createTempFile("input", ".smt2")
    val q =
      s"""|${formula.functionSymbols.flatMap(f => f.typing.output :: f.typing.input).map(s => if !supportedSorts.contains(s) then s"(define-sort ${s} () Int)" else "").mkString("", "\n", "\n")}
          |${formula.vars.map { v => s"(declare-const $v ${v.sort})" }.mkString("\n")}
          |${formula.functionSymbols.map(f => if !f.isTheory then s"(declare-fun $f ${f.typing.input.mkString("(", " ", ")")} ${f.typing.output})" else "").mkString(sep = "\n")}
          |(simplify ${formula.toStringApplicative})
          |""".stripMargin
    new PrintWriter(inputFile) {
      write(q)
      close()
    }
//    println("Query: \n" + q)
    val out0 = Seq("z3", "-smt2", inputFile.getAbsolutePath)
    val out1 = out0.!!
    val out2 = out1.linesIterator
    val out = out2.next()

    new Z3Parser(formula.functionSymbols.map(f => (f.name, f)).toMap, formula.vars.map(v => (v.name, v)).toMap).parseTerm(out) match {
      case Left(t: Term) => t
      case Right(s: String) => println(s); formula
    }
  }

  def Z3_solver_subsumption_constraints(constraints: Set[Constraint]): Set[Constraint] = {
    val constraintTerms: Set[Term] = constraints.map(_.term)
    val functionSymbols: Set[FunctionSymbol] = constraintTerms.flatMap(_.functionSymbols)
    val vars: Set[Term.Var] = constraintTerms.flatMap(_.vars)

    val inputFile: File = File.createTempFile("input", ".smt2")
    val q =
      s"""|${functionSymbols.flatMap(f => f.typing.output :: f.typing.input).map(s => if !supportedSorts.contains(s) then s"(define-sort ${s} () Int)" else "").mkString("", "\n", "\n")}
          |${vars.map { v => s"(declare-const $v ${v.sort})" }.mkString("\n")}
          |${functionSymbols.map(f => if !f.isTheory then s"(declare-fun $f ${f.typing.input.mkString("(", " ", ")")} ${f.typing.output})" else "").mkString(sep = "\n")}
          |${constraintTerms.map(t => s"(assert ${t.toStringApplicative})").mkString("\n")}
          |(apply solver-subsumption)
          |""".stripMargin
    new PrintWriter(inputFile) {
      write(q)
      close()
    }
    val outLines1 = Seq("z3", "-smt2", inputFile.getAbsolutePath).!!
    val outLines = outLines1.linesIterator.toList
    val out = outLines.slice(2, outLines.length - 2).toSet

    new Z3Parser(functionSymbols.map(f => (f.name, f)).toMap, vars.map(v => (v.name, v)).toMap).parseTerms(out) match {
      case Left(terms: Set[Term]) => terms.map(Constraint)
      case Right(s: String) => println(s); constraints
    }
  }

  def solve(formula: Term): SolverResult = {
    // The logic we use with Z3 (QF_LIA) does not support the use of undefined function symbols.
    if (formula.functionSymbols.exists(!_.isTheory)) then {
      return SolverResult.Unsatisfiable
    }
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

  // https://compsys-tools.ens-lyon.fr/z3/index.php
  private def query(query: String, produceModels: Boolean = false, logic: String = "QF_LIA"): Iterator[String] = {
    val inputFile: File = File.createTempFile("input", ".smt2")

    new PrintWriter(inputFile) {
      write(
        s"""(set-option :produce-models $produceModels)
           |
           |$query
           |""".stripMargin)
      close()
    }

//    For debugging:
//    println(inputFile.getAbsolutePath)
//    Thread.sleep(100000)

    // TODO Find a better solution than try-catch
//    try {
      val out1 = Seq("z3", "-smt2", inputFile.getAbsolutePath)
      val out2 = out1.!!
      val out3 = out2.linesIterator
      out3
//    } catch {
//      t => println(s"${PrintUtils.failureColour}Z3 error: ${t}${Console.RESET}") ; Iterator("undetermined")
//    }
  }
}
