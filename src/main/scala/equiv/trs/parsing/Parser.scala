package equiv.trs.parsing

import equiv.trs.parsing.QuasiTerm.{App, InfixChain}
import equiv.trs.*

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class ParseError(message: String)

class TRSParser(readFile: String => String) extends RegexParsers {
  // a name can consist of anything except some reserved characters '(', ')', ':', ',', ';', '[', ']'
  val name: Parser[String] = not("->") ~> """[^():,;\[\]\s]+""".r

  val query: Parser[String] = """(?s).*""".r

  val unsignedInt: Parser[Int] = """\d+""".r ^^ { _.toInt }

  // check whether some symbol is used infix, but has not been defined infix
  def undefinedInfix(signature: QuasiSignature, rules: Set[QuasiRule]): Option[String] = {
    val signatureMap = signature.leftAsMap

    // make sure all infix symbols are defined infix
    val missingInfix = rules.flatMap(_.infixOperators).find{ infix =>
      !signatureMap.contains(infix) || signatureMap(infix).infix.isEmpty
    }

    missingInfix.map { missing =>
      s"The symbol '$missing' is used as infix symbol, but has not been declared as such."
    }
  }

  def keywords: Parser[String] =
    "INCLUDE" | "DECLARE" | "WELLFOUNDED" | "CHAIN" | "SMT-RENAMINGS" | "SMT-TRANSLATIONS" |
    "THEORY" | "LOGIC" | "SOLVER" | "SIGNATURE" | "NON-STANDARD" | "IRREGULAR" | "QUERY"

  def theory: Parser[QuasiTheory] =
    rep("INCLUDE" ~> name <~ ";") ~
    ("DECLARE" ~> signature) ~
    opt("WELLFOUNDED" ~> rep1sep(name, ",") <~ ";") ~
    opt("CHAIN" ~> rep1(term ~ (":" ~> term <~ ";")) ^^ { _.map{ case left ~ right => QuasiRule(left,right,None) } }) ~
    opt("SMT-RENAMINGS" ~> renamings) ~
    opt("SMT-TRANSLATIONS" ~> translations) ^^ { case includes ~ signature ~ wellFounded ~ chains ~ smtRenamings ~ smtTranslations =>
      (QuasiTheory(QuasiSignature(signature.functions), chains.getOrElse(List.empty).toSet, smtRenamings.getOrElse(Set.empty)) :: includes.map(parseTheoryRecursive)).reduce(_.union(_))
    }

  def system: Parser[QuasiSystem] =
    ((("THEORY" ~> name <~ opt(";")) ~
      ("LOGIC" ~> name <~ opt(";")) ~
      (opt("SOLVER" ~> name <~ opt(";")) ^^ { _.getOrElse("") }) ~
      ("SIGNATURE" ~> signature)
    ) ^^ {
      case theory ~ logic ~ solver ~ signature =>
        val theories = parseTheoryRecursive(theory)
        (theory, logic, solver, signature.union(theories.signature), theories.chains)
    } ) ~
    ("RULES" ~> rules) ~
    opt("NON-STANDARD") ~
    opt("IRREGULAR") ~
    opt("QUERY" ~> query)
    ^? (
      { case (theory, logic, solver, signature, chains) ~ rules ~ nonstandard ~ irregular ~ querySimplification if undefinedInfix(signature, rules).isEmpty =>
        QuasiSystem(theory, logic, solver, signature, rules.map(_.infix2app(signature.leftAsMap)), chains.map(_.infix2app(signature.leftAsMap))) },
      { case (_, _, _, signature,_) ~ rules ~ _ ~ _ ~ _ => undefinedInfix(signature, rules).get }
    )

  // Signature definitionK
  def signature: Parser[QuasiSignature] = rep(symbolDeclaration) ^^ { typings => QuasiSignature(typings.toSet) }

  def symbolDeclaration: Parser[Either[FunctionSymbol,String]] =
    (name <~ ("," | ";") ^^ { Right(_) }) |
    (name ~ (":" ~> opt{ repsep(name, "*") <~ "=>" } ~ (name ~ infix <~ ";")) ^^ {
      case fun ~ (input ~ (output ~ infixType)) =>
        var inputSorts = input.getOrElse(List.empty).map(Sort(_))
        val variadic = inputSorts.lastOption.exists{ s => s.name.startsWith("<") && s.name.endsWith(">") }
        if(variadic) inputSorts = inputSorts.init ++ List(Sort(inputSorts.last.name.drop(1).dropRight(1)))
        inputSorts = inputSorts.map{ s => if(s == Sort("?A")) Sort.Any else s }
        val outputSort = if(output == "?A") Sort.Any else Sort(output)
        Left(FunctionSymbol(fun, Typing(inputSorts, outputSort, isVariadic = variadic), infix = infixType))
    } )

  // Infix definition
  def infix: Parser[Option[Infix]] =
    opt("(" ~> infixType ~ (unsignedInt <~ ")") ) ^^ { _.map { case kind ~ strength => Infix(kind, strength) } }

  def infixType: Parser[InfixKind] =
    "l-infix" ^^ { _ => InfixKind.Left } |
    "r-infix" ^^ { _ => InfixKind.Right } |
    "infix" ^^ { _ => InfixKind.Chain }

  // QuasiTerms
  def term: Parser[QuasiTerm] =
    termNoInfix ~ rep(name ~ termNoInfix) ^^ { case head ~ tail => InfixChain(head, tail.map{ case op ~ term => (op,term) } )}

  def termNoInfix: Parser[QuasiTerm] =
      "(" ~> (term <~ ")")
      | name ~ opt("(" ~> repsep(term, ",") <~ ")") ^^ { case name ~ args => App(name, args.getOrElse(List.empty)) }

  // Constraint
  def constraint: Parser[QuasiTerm] = opt("<--") ~> "[" ~> term <~ "]"

  // Rules
  def rules: Parser[Set[QuasiRule]] = rep(rule) ^^ { _.toSet }

  def rule: Parser[QuasiRule] =
    not(keywords) ~> term ~ ("->" ~> term) ~ (opt(constraint) <~ opt(";")) ^^ { case left ~ right ~ constraint => QuasiRule(left, right, constraint) }

  // Renamings
  def renamings: Parser[Set[Renaming]] = rep(renaming) ^^ { _.toSet }

  def renaming: Parser[Renaming] = name ~ ("->" ~> name) <~ ";" ^^ { case left ~ right => Renaming(left, right) }

  // Translations
  def translations: Parser[Set[(QuasiTerm,List[String])]] = rep(translation) ^^ { _.toSet }

  def translation: Parser[(QuasiTerm,List[String])] = term ~ ("->" ~> wrapped) <~ opt(";") ^^ { case term ~ replacement => (term,replacement) }
  def wrapped: Parser[List[String]] = "(" ~ rep(name ^^ { List(_) } | wrapped) ~ ")" ^^ { case left ~ middle ~ right => List(left) ++ middle.flatten ++ List(right) }

  def dropComments(input: String) : String = {
    input
      .replaceAll("(?s)/\\*.*?\\*/", "")
      .replaceAll("(?s)END OF FILE.*", "")
  }

  def parseTheory(input: String): Either[QuasiTheory,ParseError] = {
    parseAll[QuasiTheory](theory, dropComments(input)) match {
      case Success(x, _) => Left(x)
      case x: Failure => Right(ParseError(x.toString()))
      case x: Error => Right(ParseError(x.toString()))
    }
  }

  def parseTheoryRecursive(file : String) : QuasiTheory = {
    parseTheory(readFile(s"theories/$file.thr")) match {
      case Left(signature) => signature
      case Right(error) => throw new RuntimeException(s"Exception parsing theory $file:\n\n" + error.message)
    }
  }

  def parseSystem(input: String): Either[QuasiSystem,ParseError] = {
    parseAll[QuasiSystem](system, dropComments(input)) match {
      case Success(x, _) => Left(x)
      case x: Failure => Right(ParseError(x.toString()))
      case x: Error => Right(ParseError(x.toString()))
    }
  }

  def parseTerm(input: String): Either[QuasiTerm,ParseError] = {
    parseAll[QuasiTerm](term, input) match {
      case Success(x, _) => Left(x)
      case y => Right(ParseError(y.toString))
    }
  }
}

