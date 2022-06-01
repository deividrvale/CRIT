package equiv.trs.parsing

import equiv.trs.{FunctionSymbol, Infix, Signature, Sort, Term, Typing}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class ParseError(message: String)

trait QuasiTerm
case class QuasiInfix(head: QuasiTerm, tail: List[(String, QuasiTerm)]) extends QuasiTerm
case class QuasiApp(fun: String, args: List[QuasiTerm]) extends QuasiTerm

case class QuasiRule(left: QuasiTerm, right: QuasiTerm, constraint: Option[QuasiTerm])
case class QuasiSystem(theory: String, logic: String, solver: String, signature: Signature, rules: Set[QuasiRule])

class TRSParser() extends RegexParsers {
  // a name can consist of anything except some reserved characters '(', ')', ':', ',', ';', '[', ']'
  val name: Parser[String] = not("->") ~> """[^():,;\[\]]+""".r

  val unsignedInt: Parser[Int] = """\d+""".r ^^ { _.toInt }

  def system: Parser[QuasiSystem] =
    ("THEORY" ~> name <~ ";") ~
    ("LOGIC" ~> name <~ ";") ~
    ("SOLVER" ~> name <~ ";") ~
    ("SIGNATURE" ~> signature) ~
    ("RULES" ~> rules)
    ^^ { case theory ~ logic ~ solver ~ signature ~ rules => QuasiSystem(theory, logic, solver, signature, rules) }

  // Signature definition
  def signature: Parser[Signature] = rep(symbolDeclaration) ^^ { typings => Signature(typings.toSet) }

  def symbolDeclaration: Parser[FunctionSymbol] =
    name ~ (":" ~> opt{ repsep(name, "*") <~ "=>" } ~ (name ~ infix <~ ";")) ^^ {
      case fun ~ (input ~ (output ~ infixType)) =>
        FunctionSymbol(fun, Typing(input.getOrElse(List.empty).map(Sort(_)), Sort(output)), infix = infixType)
    }

  // Infix definition
  def infix: Parser[Option[Infix]] =
    opt("(" ~> infixType ~ (unsignedInt <~ ")") ) ^^ { _.map { case left ~ strength => Infix(left, strength) } }

  def infixType: Parser[Boolean] = ("l-infix" | "infix" | "r-infix") ^^ { _ != "r-infix" }

  // QuasiTerms
  def term: Parser[QuasiTerm] =
    termNoInfix ~ rep(name ~ termNoInfix) ^^ { case head ~ tail => QuasiInfix(head, tail.map{ case op ~ term => (op,term) } )}

  def termNoInfix: Parser[QuasiTerm] =
      "(" ~> (term <~ ")")
      | name ~ opt("(" ~> repsep(term, ",") <~ ")") ^^ { case name ~ args => QuasiApp(name, args.getOrElse(List.empty)) }

  // Constraint
  def constraint: Parser[QuasiTerm] = "[" ~> term <~ "]"

  // Rules
  def rules: Parser[Set[QuasiRule]] = rep(rule) ^^ { _.toSet }

  def rule: Parser[QuasiRule] =
    term ~ ("->" ~> term) ~ (opt(constraint) <~ ";") ^^ { case left ~ right ~ constraint => QuasiRule(left, right, constraint) }

  def parse(input: String): Either[QuasiSystem,ParseError] = {
    parseAll[QuasiSystem](system, input) match {
      case Success(x, _) => Left(x)
      case x: Failure => Right(ParseError(x.toString()))
      case x: Error => Right(ParseError(x.toString()))
    }
  }
}

object TRSParserTest {
  def main(args: Array[String]): Unit = {
    val result = new TRSParser().parse(
      """THEORY arrays ;
        |LOGIC QF_LIA ;
        |SOLVER intsolver ;
        |
        |SIGNATURE
        |  f      : Int => result       ;
        |  u1     : Int * Int => result ;
        |  u2     : Int * Int => result ;
        |  u3     : Int * Int => result ;
        |  return : Int => result       ;
        |
        |RULES
        |  f(x) -> u1(x,rnd)      [true]   ;
        |  u1(x,y) -> u1(x + 1,y) [x < 0]  ;
        |  u1(x,y) -> u2(x, y)    [x >= 0] ;
        |  u2(x,y) -> u3(x, 5)    [x = x] ;
        |  u3(x,y) -> return(y) ;
        |""".stripMargin)

    println(result)
  }
}