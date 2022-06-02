package equiv.trs.parsing

import equiv.trs.{FunctionSymbol, Infix, Signature, Sort, Term, Typing}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class ParseError(message: String)

trait QuasiTerm {
  // collect all infix operators in a QuasiTerm
  def infixOperators : Set[String] = {
    this match {
      case QuasiApp(_, args) => args.flatMap(_.infixOperators).toSet
      case QuasiInfix(head, tail) => tail.map(_._1).toSet ++ head.infixOperators ++ tail.flatMap(_._2.infixOperators)
    }
  }

  // transform all infix operators to function application
  def infix2app(signature: Map[String,FunctionSymbol]) : QuasiApp = {
    this match {
      case QuasiApp(fun, args) =>
        QuasiApp(fun, args.map(_.infix2app(signature)))
      case QuasiInfix(head, Nil) =>
        head.infix2app(signature)
      case quasiInfix @ QuasiInfix(head, tail) =>
        // find the weakest infix operators
        val infixOperators = tail.map(_._1).toSet.map{ op => (op, signature(op).infix.map(_.bindingStrength).get) }
        val weakestBinding = infixOperators.map(_._2).min
        val weakest = infixOperators.filter(_._2 == weakestBinding).map(_._1)

        // lift the weakest operators to the top
        val lifted = quasiInfix.liftOps(weakest)
        // transform the subterms
        val liftedApp = QuasiInfix(lifted.head.infix2app(signature), lifted.tail.map{ case (op,term) => (op, term.infix2app(signature)) })

        // introduce apps from left or right
        if(signature(weakest.head).infix.exists(_.isLeft)) {
          liftedApp.appFromLeft.asInstanceOf[QuasiApp]
        } else {
          liftedApp.appFromRight.asInstanceOf[QuasiApp]
        }
    }
  }
}

case class QuasiApp(fun: String, args: List[QuasiTerm]) extends QuasiTerm

// a chain of terms interspersed with infix operators
case class QuasiInfix(head: QuasiTerm, tail: List[(String, QuasiTerm)]) extends QuasiTerm {
  // lift the given operators to the 'root'
  def liftOps(ops: Set[String]): QuasiInfix = {
    tail.zipWithIndex.find { case ((op, _), _) => ops.contains(op) } match {
      case Some((_, index)) =>
        val (prefix, suffix) = tail.splitAt(index)
        val suffixLifted = QuasiInfix(suffix.head._2, suffix.tail).liftOps(ops)
        QuasiInfix( QuasiInfix(head, prefix),  (suffix.head._1, suffixLifted.head) :: suffixLifted.tail )
      case None =>
        QuasiInfix(this, List.empty)
    }
  }

  def appFromLeft : QuasiTerm = {
    if(tail.isEmpty) return head
    QuasiApp(tail.head._1, List(head, QuasiInfix(tail.head._2,tail.tail).appFromLeft))
  }

  def appFromRight : QuasiTerm = {
    if(tail.isEmpty) return head
    QuasiApp(tail.last._1, List(QuasiInfix(head,tail.init).appFromRight, tail.last._2))
  }
}

case class QuasiRule(left: QuasiTerm, right: QuasiTerm, constraint: Option[QuasiTerm]) {
  def infixOperators : Set[String] = {
    left.infixOperators ++ right.infixOperators ++ constraint.toSet.flatMap{ term => term.infixOperators }
  }

  def infix2app(signature: Map[String,FunctionSymbol]) : QuasiRule = {
    QuasiRule(left.infix2app(signature), right.infix2app(signature), constraint.map(_.infix2app(signature)))
  }
}

case class QuasiSystem(theory: String, logic: String, solver: String, signature: Signature, rules: Set[QuasiRule])

class TRSParser() extends RegexParsers {
  // a name can consist of anything except some reserved characters '(', ')', ':', ',', ';', '[', ']'
  val name: Parser[String] = not("->") ~> """[^():,;\[\]\s]+""".r

  val unsignedInt: Parser[Int] = """\d+""".r ^^ { _.toInt }

  // check whether some symbol is used infix, but has not been defined infix
  def undefinedInfix(signature: Signature, rules: Set[QuasiRule]): Option[String] = {
    val signatureMap = signature.functions.map{ fun => fun.name -> fun }.toMap

    // make sure all infix symbols are defined infix
    val missingInfix = rules.flatMap(_.infixOperators).find{ infix => !signatureMap.contains(infix) || signatureMap(infix).infix.isEmpty }

    missingInfix.map { missing =>
      s"The symbol '$missing' is used as infix symbol, but has not been declared as such."
    }
  }

  def system: Parser[QuasiSystem] =
    ("THEORY" ~> name <~ ";") ~
    ("LOGIC" ~> name <~ ";") ~
    ("SOLVER" ~> name <~ ";") ~
    ("SIGNATURE" ~> signature) ~
    ("RULES" ~> rules)
    ^? (
      { case theory ~ logic ~ solver ~ signature ~ rules if undefinedInfix(signature, rules).isEmpty =>
        val signatureMap = signature.functions.map{ fun => fun.name -> fun }.toMap
        QuasiSystem(theory, logic, solver, signature, rules.map(_.infix2app(signatureMap))) },
      { case _ ~ _ ~ _ ~ signature ~ rules => undefinedInfix(signature, rules).get }
    )

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