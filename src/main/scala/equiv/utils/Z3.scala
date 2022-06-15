package equiv.utils

import equiv.trs.{FunctionSymbol, Sort, Term, Typing}

import java.io.{File, PrintWriter}
import sys.process.*

enum SolverResult:
  case Satisfiable, Unsatisfiable, Undetermined

object Z3 {
  def main(args: Array[String]): Unit = {
    def gt(x: Term, y: Term) = Term.App(FunctionSymbol(">", Typing(List(Sort.Int, Sort.Int), Sort.Bool)), List(x,y))
    def and(x: Term, y: Term) = Term.App(FunctionSymbol("and", Typing(List(Sort.Bool, Sort.Bool), Sort.Bool)), List(x,y))
    def variable(v: String) = Term.Var(v, Sort.Int)
    def int(x: Int) = Term.App(FunctionSymbol(x.toString, Typing(List.empty, Sort.Int)))

    val x = variable("x")

    List(
      gt(x, int(5)),
      and( gt(x, int(5)), gt(int(2), x) ),
    ).foreach{ formula =>
      println(formula)
      println(solve(formula))
    }
  }

  def solve[T](formula: Term) : SolverResult = {
    val variables = formula.vars
    val produceModels = false
    val logic = "QF_LIA"
    val inputFile = File.createTempFile("input", ".smt2")

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

    /* For debugging:
    println(inputFile.getAbsolutePath)
    Thread.sleep(100000)
    */
    val output = Seq("z3", "-smt2", inputFile.getAbsolutePath).!!.linesIterator

    output.next() match {
      case "sat" => SolverResult.Satisfiable
      case "unsat" => SolverResult.Unsatisfiable
      case _ => SolverResult.Undetermined
    }
  }
}
