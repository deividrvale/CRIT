package equiv.utils

import equiv.trs.{FunctionSymbol, Sort, Term, Typing}

import java.io.{File, PrintWriter}
import sys.process.*

enum SolverResult:
  case Satisfiable, Unsatisfiable, Undetermined

object Z3 {
  def main(args: Array[String]): Unit = {
    def gt(x: Term, y: Term) = Term.App(FunctionSymbol(">", Typing(List(Sort.Int, Sort.Int), Sort.Bool)), List(x,y))
    def lt(x: Term, y: Term) = Term.App(FunctionSymbol("<", Typing(List(Sort.Int, Sort.Int), Sort.Bool)), List(x,y))
    def and(x: Term, y: Term) = Term.App(FunctionSymbol("and", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool)), List(x,y))
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
  def constraintImplication(term1: Term, term2: Term): Boolean = {
    val formula = TermUtils.not(TermUtils.impl(term1, term2))
    solve(formula) == SolverResult.Unsatisfiable
  }

  /** @return Whether the first term implies the second and the second does not imply the first */
  def constraintStrictImplication(term1: Term, term2: Term): Boolean = {
    val formula = TermUtils.not(TermUtils.and(TermUtils.impl(term1, term2), TermUtils.not(TermUtils.impl(term2, term1))))
    solve(formula) == SolverResult.Unsatisfiable
  }

  /** @return Whether the first term implies the second and the second implies the first */
  def constraintBiImplication(term1: Term, term2: Term): Boolean = {
    val formula = TermUtils.not(TermUtils.biImpl(term1, term2))
    solve(formula) == SolverResult.Unsatisfiable
  }

  /** Check if a term is satisfiable */
  def isSatisfiable(term: Term): Boolean = {
    solve(term) == SolverResult.Satisfiable
  }

  def solve[T](formula: Term) : SolverResult = {
    val variables: Set[Term.Var] = formula.vars
    val produceModels: Boolean = false
    val logic: String = "QF_LIA"
    val inputFile: File = File.createTempFile("input", ".smt2")

    new PrintWriter(inputFile) {
      write(
        s"""(set-option :produce-models $produceModels)
           |(set-logic $logic)
           |
           |${variables.map{ v => s"(declare-fun $v () Int)" }.mkString("\n")}
           |
           |(assert
           |  ${formula.toStringApplicative}
           |)
           |
           |(check-sat)
           |""".stripMargin
      )
      close()
    }

//    For debugging:
//    println(inputFile.getAbsolutePath)
//    Thread.sleep(100000)

    val output: Iterator[String] = Seq("z3", "-smt2", inputFile.getAbsolutePath).!!.linesIterator

    output.next() match {
      case "sat" => SolverResult.Satisfiable
      case "unsat" => SolverResult.Unsatisfiable
      case _ => SolverResult.Undetermined
    }
  }
}
