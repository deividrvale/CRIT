package equiv.trs.parsing

import equiv.trs.Term.{App, Var}
import equiv.trs.parsing.QuasiTerm.InfixChain
import equiv.trs.{FunctionSymbol, Sort, Term, Typing}
import equiv.utils.{TermUtils, TheorySymbols}

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class Z3Parser(termString: String, functionSymbolsMap: Map[String, FunctionSymbol], variablesMap: Map[String, Var]) extends RegexParsers {

  def parseTerm(): Either[Term, String] = {
    parseAll[Term](termParser, termString) match {
      case Success(t: Term, _) => Left(t)
      case y => Right(y.toString)
    }
  }

  def termParser: Parser[Term] =
    "(" ~> name ~ rep(termParser) <~ ")" ^^ { case nam ~ terms => stringToTerm(nam, terms) }
    | name ^^ { nam => stringToTerm(nam, List()) }

  val name: Parser[String] = not("->") ~> """[^():,;\[\]\s]+""".r

  def stringToTerm(name: String, arguments: List[Term]): Term = {
    if variablesMap.contains(name) then variablesMap(name)
    else if functionSymbolsMap.contains(name) then parseVariadic(functionSymbolsMap(name), arguments)
      // Unknown name, so must be a new value Int after simplification
    else if TheorySymbols.theorySymbols.contains(name) then parseVariadic(TheorySymbols.theorySymbols(name), arguments)
    else TermUtils.maybeGetValue(name).orNull // TODO maybe create exception or handle other values or Option
  }

  def parseVariadic(functionSymbol: FunctionSymbol, arguments: List[Term]): Term = {
    val n = functionSymbol.typing.input.length
    if arguments.length == n then App(functionSymbol, arguments)
    else
      val firstNArguments = arguments.take(n)
      parseVariadic(functionSymbol, App(functionSymbol, firstNArguments) :: arguments.drop(n) )
  }
}
