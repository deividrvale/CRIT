package equiv

import equiv.ri.Equation
import equiv.sample.SampleObjects
import equiv.trs.parsing.QuasiTerm.{App, InfixChain}
import equiv.trs.*
import equiv.trs.parsing.{QuasiSystem, QuasiTerm, QuasiTheory, TRSParser, Z3Parser}
import equiv.trs.{FunctionSymbol, Sort, Term, Typing, parsing}

import java.io.{File, PrintWriter}
import sys.process.*

enum SolverResult:
  case Satisfiable, Unsatisfiable, Undetermined

object Z3test {
  def main(args: Array[String]): Unit = {
    simplifyTerm(SampleObjects.deletionEquation2.constraints.head.term)
  }

  def supportedSorts : List[Sort] = List(Sort.Int, Sort.Bool)

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
      s"""|${formula.functionSymbols.flatMap(f => f.typing.output :: f.typing.input).map(s => if !supportedSorts.contains(s) then s"(define-sort ${s} () Int)" else "").mkString("", "\n", "\n")}
          |${formula.vars.map { v => s"(declare-const $v ${v.sort})" }.mkString("\n")}
          |${formula.functionSymbols.map(f => if !f.isTheory then s"(declare-fun $f ${f.typing.input.mkString("(", " ", ")")} ${f.typing.output})" else "").mkString(sep = "\n")}
          |(simplify ${formula.toStringApplicative})
          |""".stripMargin
    val q2 =
    """(declare-const a Int)
      |(declare-const b Int)
      |(declare-const c Int)
      |(declare-const d Int)
      |(assert (and (> d 1) (> d 2)))
      |(assert (and (and (and (> b 2) (< b 4)) (and (> c 2) (> c 4))) (and (> a 0) (> a 1))))
      |(apply solver-subsumption)
      |""".stripMargin

    val tactics = List("ctx-solver-simplify", "solver-subsumption")
    var bla = "(> (+ (-1) (+ x (-1))) 0)"
    bla = "(> (+ (+ (- x 1) -1) -1) 0)"
    val q3 =
      s"""(declare-const x Int)
         |(assert $bla)
         |(apply solver-subsumption)""".stripMargin

    val q4 =
      s"""(declare-const x Int)
          |(simplify (+ (+ x 1) 1))
          |""".stripMargin

    val q5 =
      s"""(declare-const b Bool)
         |(simplify true)""".stripMargin

    val query = q5
    new PrintWriter(inputFile) {
      write(query)
      close()
    }
    println(query)

    println("RESULT: ---------------------------")

    val out1: Seq[String] = Seq("z3", "-smt2", inputFile.getAbsolutePath)
    val out: String = out1.!!
    //    val out3: Iterator[String] = out2.linesIterator
    //    val out: String = out3.next()

    println(out)
    sample.SampleObjects.termFx

//    new Z3Parser(out, formula.functionSymbols.map(f => (f.name, f)).toMap, formula.vars.map(v => (v.name, v)).toMap).parseTerm() match {
//      case Left(t: Term) => t
//      case Right(s: String) => println(s); formula
//    }
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
//        println(inputFile.getAbsolutePath)
//        Thread.sleep(100000)

    // TODO Find a better solution than try-catch
    //    try {
    Seq("z3", "-smt2", inputFile.getAbsolutePath).!!.linesIterator
    //    } catch {
    //      t => println(s"${PrintUtils.failureColour}Z3 error: ${t}${Console.RESET}") ; Iterator("undetermined")
    //    }
  }
}
