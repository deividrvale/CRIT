package equiv.utils

import equiv.trs.parsing.QuasiTerm.{App, InfixChain}
import equiv.trs.*

import equiv.trs.Temp.SumUp.equation
import equiv.trs.parsing.{QuasiSystem, QuasiTerm, QuasiTheory, TRSParser}
import equiv.trs.{FunctionSymbol, Sort, Term, Typing, parsing}

import java.io.{File, PrintWriter}
import sys.process.*

enum SolverResult:
  case Satisfiable, Unsatisfiable, Undetermined

object Z3 {
  def main(args: Array[String]): Unit = {
    val equation = equiv.trs.Temp.SumUp.eqT2

    simplify(equation.left)
    println("========================================================")
    simplify(equation.right)
    println("========================================================")
    simplify(equation.getConstrainsConjunctAsTerm)
    println("========================================================")
  }

  def solveMain(): Unit = {
    def gt(x: Term, y: Term) = Term.App(FunctionSymbol(">", Typing(List(Sort.Int, Sort.Int), Sort.Bool)), List(x, y))
    def lt(x: Term, y: Term) = Term.App(FunctionSymbol("<", Typing(List(Sort.Int, Sort.Int), Sort.Bool)), List(x, y))
    def and(x: Term, y: Term) = Term.App(FunctionSymbol("and", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool)), List(x, y))
    def impl(x: Term, y: Term) = Term.App(FunctionSymbol("=>", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool)), List(x, y))
    def variable(v: String) = Term.Var(v, Sort.Int)
    def int(x: Int) = Term.App(FunctionSymbol(x.toString, Typing(List.empty, Sort.Int)))
    val x = variable("x")

    List(
      lt(x, int(-1)),
      gt(int(2), int(5)),
      gt(int(5), int(5)),
    ).foreach{ formula =>
      println(formula)
      println(solve(formula))
    }
  }

  /** @return Whether the first term implies the second */
  def implies(term1: Term, term2: Term): Boolean = {
    val formula = TermUtils.not(TermUtils.impl(term1, term2))
    try { solve(formula) == SolverResult.Unsatisfiable }
    finally { return false }
  }

  /** @return Whether the first term implies the second and the second implies the first */
  def constraintBiImplication(term1: Term, term2: Term): Boolean = {
    val formula = TermUtils.not(TermUtils.biImpl(term1, term2))
    solve(formula) == SolverResult.Unsatisfiable
  }

  /** Check if a term is satisfiable */
  def satisfiable(term: Term): Boolean = {
    solve(term) == SolverResult.Satisfiable
  }

  /** TODO */
  def simplify(formula: Term): Option[Term] = {
    val inputFile: File = File.createTempFile("input", ".smt2")
    val q =
      s"""|${formula.vars.map{ v => s"(declare-const $v ${v.sort})" }.mkString("\n")}
          |${formula.functionSymbols.map(f => if !f.isTheory then s"(declare-fun $f ${f.typing.input.mkString("(", " ", ")")} ${f.typing.output})" else "").mkString(sep="\n")}
          |(simplify ${formula.toStringApplicative})
          |""".stripMargin
    new PrintWriter(inputFile) {
      write(q)
      close()
    }
    val out: String = Seq("z3", "-smt2", inputFile.getAbsolutePath).!!.linesIterator.next()

    println(out)

    val parser: TRSParser = new TRSParser(_ => out)
    val parseResult: Option[QuasiTerm] = parser.parseTerm(out) match {
      case Left(x) => Some(x) //Some(x.toTerm(formula.functionSymbols.map(f => (f.name, f)).toMap, formula.vars.map(v => (v.name, v.sort)).toMap))
      case Right(x) => println(x) ; None
    }
    println(parseResult)
    Some(Term.Var("x", Sort.Bool))
  }

  def solve(formula: Term) : SolverResult = {
    val variables: Set[Term.Var] = formula.vars
    val output: Iterator[String] = query(
      s"""${variables.map{ v => s"(declare-fun $v () Int)" }.mkString("\n")}
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

    ////    For debugging:
    //    println(inputFile.getAbsolutePath)
    //    Thread.sleep(100000)

    Seq("z3", "-smt2", inputFile.getAbsolutePath).!!.linesIterator
  }
}
