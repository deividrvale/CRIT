package equiv.trs.parsing

import equiv.trs.{FunctionSymbol, Infix, Signature, Sort, Typing}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class ParseError(message: String)

case class QuasiTerm()
case class QuasiRule(left: QuasiTerm, right: QuasiTerm, constraint: QuasiTerm)
case class QuasiSystem(theory: String, logic: String, solver: String, signature: Signature, rules: Set[QuasiRule])

class TRSParser() extends RegexParsers {
  val name: Parser[String] = """\w[\w\d]+""".r
  val number: Parser[String] = """\d+""".r

  def system: Parser[QuasiSystem] =
    ("THEORY" ~> name) ~
    ("LOGIC" ~> name) ~
    ("SOLVER" ~> name) ~
    ("SIGNATURE" ~> signature) ~
    ("RULES" ~> rules)
    ^^ { case theory ~ logic ~ solver ~ signature ~ rules => QuasiSystem(theory, logic, solver, signature, rules) }

  def signature: Parser[Signature] = rep(symbolDeclaration) ^^ { typings => Signature(typings.toSet) }

  def symbolDeclaration: Parser[FunctionSymbol] =
    name ~ (":" ~> opt{ repsep(name, "*") <~ "=>" } ~ (name ~ infix <~ ";")) ^^ {
      case fun ~ (input ~ (output ~ infixType)) =>
        FunctionSymbol(fun, Typing(input.getOrElse(List.empty).map(Sort(_)), Sort(output)), infix = infixType)
    }

  def infix: Parser[Option[Infix]] =
    opt("(" ~> infixType ~ (number <~ ")") ) ^^ { _.map { case left ~ strength => Infix(left, strength.toInt) } }

  def infixType: Parser[Boolean] =
    ("l-infix" | "infix" | "r-infix") ^^ { _ != "r-infix" }

  def rules: Parser[Set[QuasiRule]] = ???

  def parse(input: String): Either[QuasiSystem,ParseError] = {
    parseAll[QuasiSystem](system, input) match {
      case Success(x, _) => Left(x)
      case x: Failure => Right(ParseError(x.toString()))
      case x: Error => Right(ParseError(x.toString()))
    }
  }
}
/*
THEORY arrays ;
LOGIC QF_LIA ;
SOLVER intsolver ;

SIGNATURE
  f      : Int => result       ;
  u1     : Int * Int => result ;
  u2     : Int * Int => result ;
  u3     : Int * Int => result ;
  return : Int => result       ;

RULES
  f(x) -> u1(x,rnd)      [true]   ;
  u1(x,y) -> u1(x + 1,y) [x < 0]  ;
  u1(x,y) -> u2(x, y)    [x >= 0] ;
  u2(x,y) -> u3(x, 5)    [x = x] ;
  u3(x,y) -> return(y) ;

*/