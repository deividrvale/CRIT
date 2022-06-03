package equiv.trs.parsing

import equiv.trs.{App, Constraint, FunctionSymbol, Rule, Signature, Sort, System, Term, Typing, Var}

trait QuasiTerm {
  // collect all infix operators in a QuasiTerm
  def infixOperators : Set[String] = {
    this match {
      case QuasiApp(_, args) => args.flatMap(_.infixOperators).toSet
      case QuasiInfix(head, tail) => tail.map(_._1).toSet ++ head.infixOperators ++ tail.flatMap(_._2.infixOperators)
    }
  }

  // function symbols
  def functionSymbols : Set[(String,Int)] = {
    this match {
      case QuasiApp(fun, args) => Set((fun,args.length)) ++ args.flatMap(_.functionSymbols)
      case QuasiInfix(head, tail) => (head :: tail.map(_._2)).flatMap(_.functionSymbols).toSet
    }
  }

  // transform all infix operators to function application
  def infix2app(signature: Map[String,FunctionSymbol]) : QuasiApp = {
    this match {
      case QuasiApp(fun, args) =>
        QuasiApp(fun, args.map(_.infix2app(signature)))
      case QuasiInfix(head, Nil) =>
        head.infix2app(signature)
      case quasiInfix @ QuasiInfix(_, tail) =>
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

  def toTerm(signature: Map[String,FunctionSymbol], variableSorts: Map[String,Sort]) : Term = {
    this match {
      case QuasiApp(fun, args) =>
        signature.get(fun) match {
          case Some(symbol) =>
            App(symbol, args.map(_.toTerm(signature, variableSorts)))
          case None =>
            if(args.nonEmpty) {
              throw new RuntimeException(s"The type of symbol $fun is unknown.")
            } else {
              Var(fun, variableSorts(fun))
            }
        }
      case _ => throw new RuntimeException("The conversion from QuasiTerm to Term is only allows after all QuasiInfix have been eliminated.")
    }
  }
}

case class QuasiApp(fun: String, args: List[QuasiTerm]) extends QuasiTerm {
  override def toString: String = {
    s"$fun${if(args.nonEmpty) args.mkString("( ", ", ", " )") else ""}"
  }
}

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

  def functionSymbols : Set[(String,Int)] = {
    left.functionSymbols ++ right.functionSymbols ++ constraint.toSet.flatMap{ term => term.functionSymbols }
  }

  def infix2app(signature: Map[String,FunctionSymbol]) : QuasiRule = {
    QuasiRule(left.infix2app(signature), right.infix2app(signature), constraint.map(_.infix2app(signature)))
  }

  def toRule(signature: Map[String,FunctionSymbol], variableSorts: Map[String,Sort]) : Rule = {
    Rule(left.toTerm(signature,variableSorts), right.toTerm(signature,variableSorts), constraint.map{ c => Constraint(c.toTerm(signature,variableSorts)) })
  }

  override def toString: String = {
    s"$left -> $right ${constraint.map{ c => s"[ $c ]" }.getOrElse("")}"
  }
}

case class QuasiSystem(theory: String, logic: String, solver: String, signature: Signature, rules: Set[QuasiRule]) {
  def deriveTypings: (Signature, Map[(QuasiRule,String),Sort]) = {
    // a function argument (Some(nr)) or output (None)
    type Port = (Any,Option[Int])

    // function symbols with arities
    val symbol2arity =
      (signature.functions.map{ symbol => (symbol.name, symbol.typing.input.length) } ++ rules.flatMap(_.functionSymbols).filter(_._2 > 0))
        .groupBy(_._1).map { case (symbol, group) =>
          if (group.size == 1) {
            (symbol, group.head._2)
          } else throw new RuntimeException(s"The symbol ${symbol} occurs with varying arities ${group.map(_._2).mkString(",")}")
        }

    def var2port(rule: QuasiRule, name: String) : ((QuasiRule,String),Option[Int]) = ((rule,name),None)
    val variables : Set[((QuasiRule,String),Option[Int])] = rules.flatMap{ rule =>
      rule.functionSymbols.filter{ symbol => symbol._2 == 0 && !symbol2arity.contains(symbol._1) }.map{ symbol => var2port(rule,symbol._1) }
    }

    var port2class : Map[Port,Int] = (symbol2arity.flatMap{ case (symbol,arity) =>
      (symbol,None) :: (0 until arity).toList.map{ i => (symbol,Some(i)) }
    } ++ variables).zipWithIndex.toMap

    def setPortsEqual(a: Port, b: Port): Unit = {
      val aclass = port2class(a)
      val bclass = port2class(b)
      if(aclass != bclass) port2class = port2class.view.mapValues{ v => if(v == aclass) bclass else v }.toMap
    }

    def deriveEquality(rule: QuasiRule, term: QuasiTerm) : Port = {
      term match {
        case QuasiApp(fun, args) =>
          args.indices.foreach{ i => setPortsEqual(deriveEquality(rule, args(i)), (fun,Some(i))) }
          if(symbol2arity.contains(fun)) (fun,None) else var2port(rule,fun)
      }
    }

    rules.foreach{ rule =>
      setPortsEqual(deriveEquality(rule, rule.left), deriveEquality(rule, rule.right))
      rule.constraint.foreach(deriveEquality(rule,_))
    }

    // derive types
    var class2sort : Map[Int,Sort] = Map.empty

    def setClass2Sort(clazz: Int, sort: Sort): Unit = {
      if(class2sort.get(clazz).exists(_ != sort)) {
        throw new RuntimeException(s"There is a typing conflict ${class2sort(clazz)} != $sort.")
      }
      class2sort = class2sort.updated(clazz, sort)
    }

    signature.functions.foreach{ symbol =>
      setClass2Sort(port2class((symbol.name,None)), symbol.typing.output)
      symbol.typing.input.indices.foreach{ i =>
        setClass2Sort(port2class((symbol.name,Some(i))), symbol.typing.input(i))
      }
    }

    // typing for each symbol
    (
      Signature(symbol2arity.map { case (symbol,arity) =>
        val inputSorts = (0 until arity).toList.map{ i =>
          class2sort.get(port2class((symbol,Some(i)))) match {
            case Some(sort) => sort
            case None => throw new RuntimeException(s"Failed to derive the type of $symbol argument $i.")
          }
        }

        val outputSort = class2sort.get(port2class((symbol,None))) match {
          case Some(sort) => sort
          case None => throw new RuntimeException(s"Failed to derive the output sort of $symbol.")
        }

        FunctionSymbol(symbol, Typing(inputSorts, outputSort), signature.functions.find(_.name == symbol).flatMap(_.infix))
      }.toSet),
      variables.map{ case port @ ((rule,name),_) => (rule,name) -> class2sort(port2class(port)) }.toMap
    )
  }

  def toSystem: System = {
    val (signature, variableSortsAllRules) = deriveTypings
    val signatureMap = signature.asMap
    System(theory, logic, solver, signature, rules = rules.map { rule =>
      val variableSorts = variableSortsAllRules.filter(_._1._1 == rule).map { case ((_, name), sort) => name -> sort }
      rule.toRule(signatureMap, variableSorts)
    })
  }

  override def toString: String = {
    s"""SIGNATURE:
       |  ${signature.functions.mkString("\n  ")}
       |
       |RULES:
       |  ${rules.mkString("\n  ")}
       |""".stripMargin
  }
}
