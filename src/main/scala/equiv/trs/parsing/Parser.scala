package equiv.trs.parsing

import equiv.trs.{FunctionSymbol, Infix, Renaming, Signature, Sort, Term, Theory, Typing}

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class ParseError(message: String)

class TRSParser(readFile: String => String) extends RegexParsers {
  // a name can consist of anything except some reserved characters '(', ')', ':', ',', ';', '[', ']'
  val name: Parser[String] = not("->") ~> """[^():,;\[\]\s]+""".r

  val querySimplification: Parser[String] = """\[.*?]""".r

  val unsignedInt: Parser[Int] = """\d+""".r ^^ { _.toInt }

  // check whether some symbol is used infix, but has not been defined infix
  def undefinedInfix(signature: Signature, rules: Set[QuasiRule]): Option[String] = {
    val signatureMap = signature.asMap

    // make sure all infix symbols are defined infix
    val missingInfix = rules.flatMap(_.infixOperators).find{ infix => !signatureMap.contains(infix) || signatureMap(infix).infix.isEmpty }

    missingInfix.map { missing =>
      s"The symbol '$missing' is used as infix symbol, but has not been declared as such."
    }
  }

  def theory: Parser[Theory] =
    rep("INCLUDE" ~> name <~ ";") ~
    ("DECLARE" ~> signature) ~
    opt("WELLFOUNDED" ~> rep1sep(name, ",") <~ ";") ~
    opt("CHAIN" ~> rep1(term ~ (":" ~> term <~ ";"))) ~
    opt("SMT-RENAMINGS" ~> renamings) ~
    opt("SMT-TRANSLATIONS" ~> translations) ^^ { case includes ~ signature ~ wellFounded ~ chains ~ smtRenamings ~ smtTranslations =>
      (Theory(Signature(signature.functions), smtRenamings.getOrElse(Set.empty)) :: includes.map(readTheoryRecursive)).reduce(_.union(_))
    }

  def system: Parser[QuasiSystem] =
    ((("THEORY" ~> name <~ ";") ~
      ("LOGIC" ~> name <~ ";") ~
      ("SOLVER" ~> name <~ ";") ~
      ("SIGNATURE" ~> signature)
    ) ^^ {
      case theory ~ logic ~ solver ~ signature =>
        (theory, logic, solver, signature.union(readTheoryRecursive(theory).signature))
    } ) ~
    ("RULES" ~> rules) ~
    opt("QUERY simplification" ~> querySimplification)
    ^? (
      { case (theory, logic, solver, signature) ~ rules ~ querySimplification if undefinedInfix(signature, rules).isEmpty =>
        QuasiSystem(theory, logic, solver, signature, rules.map(_.infix2app(signature.asMap))) },
      { case (_, _, _, signature) ~ rules ~ _ => undefinedInfix(signature, rules).get }
    )

  // Signature definitionK
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

  // Renamings
  def renamings: Parser[Set[Renaming]] = rep(renaming) ^^ { _.toSet }

  def renaming: Parser[Renaming] = name ~ ("->" ~> name) <~ ";" ^^ { case left ~ right => Renaming(left, right) }

  // Translations
  def translations: Parser[Set[(QuasiTerm,List[String])]] = rep(translation) ^^ { _.toSet }

  def translation: Parser[(QuasiTerm,List[String])] = term ~ ("->" ~> wrapped) <~ opt(";") ^^ { case term ~ replacement => (term,replacement) }
  def wrapped: Parser[List[String]] = "(" ~ rep(name ^^ { List(_) } | wrapped) ~ ")" ^^ { case left ~ middle ~ right => List(left) ++ middle.flatten ++ List(right) }

  def dropCommnets(input: String) : String = {
    input
      .replaceAll("(?s)/\\*.*?\\*/", "")
      .replaceAll("(?s)END OF FILE.*", "")
  }

  def parseTheory(input: String): Either[Theory,ParseError] = {
    parseAll[Theory](theory, dropCommnets(input)) match {
      case Success(x, _) => Left(x)
      case x: Failure => Right(ParseError(x.toString()))
      case x: Error => Right(ParseError(x.toString()))
    }
  }

  def readTheoryRecursive(file : String) : Theory = {
    parseTheory(readFile(s"theories/$file.thr")) match {
      case Left(signature) => signature
      case Right(error) => throw new RuntimeException(s"Exception parsing theory $file:\n\n" + error.message)
    }
  }

  def parseSystem(input: String): Either[QuasiSystem,ParseError] = {
    parseAll[QuasiSystem](system, dropCommnets(input)) match {
      case Success(x, _) => Left(x)
      case x: Failure => Right(ParseError(x.toString()))
      case x: Error => Right(ParseError(x.toString()))
    }
  }
}

object TRSParserTest {
  def main(args: Array[String]): Unit = {
    new TRSParser(readFile).parseSystem(readFile("examples/declare.ctrs")) match {
      case Left(quasiSystem) =>
        println(quasiSystem)
        val system = quasiSystem.toSystem
        println(system)
      case Right(error) => println(error.message)
    }
  }

  def readFile(file: String) : String = {
    Source.fromResource(file).getLines().mkString("\n")
  }
}