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
  def main(args: Array[String]): Unit = {
//    solveMain()
    simplala()
  }

  def simplala(): Unit = {
    val equa: Equation = equiv.trs.Temp.SumUp.equation
    println(equa.toPrintString())
    simplifyEquation(equa)
  }

  def solveMain(): Unit = {
    def gt(x: Term, y: Term) = Term.App(FunctionSymbol(">", Typing(List(Sort.Int, Sort.Int), Sort.Bool)), List(x, y))

    def lt(x: Term, y: Term) = Term.App(FunctionSymbol("<", Typing(List(Sort.Int, Sort.Int), Sort.Bool)), List(x, y))

    def and(x: Term, y: Term) = Term.App(FunctionSymbol("or", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool)), List(x, y))

    def impl(x: Term, y: Term) = Term.App(FunctionSymbol("=>", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool)), List(x, y))

    def variable(v: String) = Term.Var(v, Sort.Int)

    def int(x: Int) = Term.App(FunctionSymbol(x.toString, Typing(List.empty, Sort.Int)))

    val x = variable("x")

    List(
      and(lt(x, int(-1)),
        gt(int(2), int(5))),
      gt(int(5), int(5)),
    ).foreach { formula =>
      println(formula)
      println(solve(formula))
    }
  }

  /** @return Whether the first term implies the second */
  def implies(term1: Term, term2: Term): Boolean = {
    val formula = TheorySymbols.notX(TheorySymbols.implXY(term1, term2))
    solve(formula) == SolverResult.Unsatisfiable
  }

  /** @return Whether the first term implies the second and the second implies the first */
  def constraintBiImplication(term1: Term, term2: Term): Boolean = {
    val formula = TheorySymbols.notX(TheorySymbols.biImplXY(term1, term2))
    solve(formula) == SolverResult.Unsatisfiable
  }

  /** Check if a term is satisfiable */
  def satisfiable(term: Term): Boolean = {
    solve(term) == SolverResult.Satisfiable
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
    val variables: Set[Term.Var] = formula.vars
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

    Seq("z3", "-smt2", inputFile.getAbsolutePath).!!.linesIterator
  }
}
