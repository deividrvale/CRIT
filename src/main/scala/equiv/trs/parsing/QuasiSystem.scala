package equiv.trs.parsing

import equiv.trs.parsing.QuasiTerm.{App, InfixChain}
import equiv.trs.{FunctionSymbol, Signature, Sort, System, Typing}

case class QuasiSystem(theory: String, logic: String, solver: String, signature: QuasiSignature, rules: Set[QuasiRule], chains: Set[QuasiRule]) {
  def deriveTypings: (Signature, Map[(QuasiRule, String), Sort]) = {
    // a function argument (Some(nr)) or output (None)
    type Port = (Any, Option[Int])

    val symbols = signature.asMap
    val symbolsTyped = signature.leftAsMap

    // function symbols with arities
    var symbol2arity = signature.left.map { symbol => symbol.name -> (symbol.typing.input.length, symbol.typing.isTheory, symbol.typing.isVariadic) }.toMap
    rules.flatMap(_.functionSymbols).filter { s => symbols.contains(s._1) || s._2 > 0 }.foreach { case (symbol, arity) =>
      if (symbol2arity.contains(symbol)) {
        val (theArity, theory, variadic) = symbol2arity(symbol)
        if (theArity != arity && !variadic) throw new RuntimeException(s"The symbol ${symbol} occurs with varying arities.")
      } else {
        if (!symbol2arity.contains(symbol)) symbol2arity = symbol2arity.updated(symbol, (arity, false, false))
      }
    }

    // symbol arguments as ports
    def portOfSymbol(symbol: String, arg: Option[Int]): Port = {
      arg match {
        case Some(argNr) => (symbol, Some(Math.min(argNr, symbol2arity(symbol)._1 - 1)))
        case None => (symbol, None)
      }
    }

    // variables as ports
    def portOfVariable(rule: QuasiRule, name: String): ((QuasiRule, String), Option[Int]) = ((rule, name), None)

    val variables: Set[((QuasiRule, String), Option[Int])] = rules.flatMap { rule =>
      rule.functionSymbols.filter { symbol => symbol._2 == 0 && !symbol2arity.contains(symbol._1) }.map { symbol => portOfVariable(rule, symbol._1) }
    }

    // checks if the given argument is polymorphic
    def isSortAny(symbol: String, arg: Option[Int]): Boolean = {
      symbolsTyped.get(symbol).exists { functionSymbol =>
        val typing = functionSymbol.typing
        portOfSymbol(symbol, arg) match {
          case (_, Some(nr)) => typing.input(nr) == Sort.Any
          case (_, None) => typing.output == Sort.Any
        }
      }
    }

    // ports (excluding sort Any)
    var port2class: Map[Port, Int] = (symbol2arity.toList.flatMap { case (symbol, (arity, _, _)) =>
      ((symbol, None) :: (0 until arity).toList.map { i => (symbol, Some(i)) }).filterNot(isSortAny)
    } ++ variables).zipWithIndex.toMap

    def setPortsEqual(a: Port, b: Port): Unit = {
      val aclass = port2class(a)
      val bclass = port2class(b)
      if (aclass != bclass) port2class = port2class.view.mapValues { v => if (v == aclass) bclass else v }.toMap
    }

    def deriveEquality(rule: QuasiRule, term: QuasiTerm): Port = {
      term match {
        case App(symbol, args) =>
          val argPorts: List[Port] = args.indices.map { i => deriveEquality(rule, args(i)) }.toList
          // not Any
          args.indices.foreach { i =>
            if (!isSortAny(symbol, Some(i))) setPortsEqual(argPorts(i), (symbol, Some(i)))
          }
          // Any
          val argPortsAny: List[Port] = args.indices.filter { i => isSortAny(symbol, Some(i)) }.map(argPorts).toList
          if (argPortsAny.nonEmpty) {
            argPortsAny.sliding(2, 1).foreach {
              case List(a, b) => setPortsEqual(a, b)
              case _ =>
            }
          }

          if (symbol2arity.contains(symbol)) {
            if (isSortAny(symbol, None)) argPortsAny.head else (symbol, None)
          } else portOfVariable(rule, symbol)
      }
    }

    rules.foreach { rule =>
      setPortsEqual(deriveEquality(rule, rule.left), deriveEquality(rule, rule.right))
      rule.constraint.foreach(deriveEquality(rule, _))
    }

    // derive types
    var class2sort: Map[Int, Sort] = Map.empty

    def setClass2Sort(clazz: Int, sort: Sort): Unit = {
      if (class2sort.get(clazz).exists(_ != sort)) {
        throw new RuntimeException(s"There is a typing conflict ${class2sort(clazz)} != $sort.")
      }
      class2sort = class2sort.updated(clazz, sort)
    }

    signature.left.foreach { symbol =>
      if (!isSortAny(symbol.name, None)) setClass2Sort(port2class((symbol.name, None)), symbol.typing.output)
      symbol.typing.input.indices.foreach { i =>
        if (!isSortAny(symbol.name, Some(i))) setClass2Sort(port2class((symbol.name, Some(i))), symbol.typing.input(i))
      }
    }

    // set missing types to "result"
    val default = Sort("result")
    symbol2arity.foreach { case (symbol, (arity, theory, variadic)) =>
      (0 until arity).foreach { i =>
        if (isSortAny(symbol, Some(i))) Sort.Any
        else class2sort.get(port2class((symbol, Some(i)))) match {
          case None => class2sort = class2sort.updated(port2class((symbol, Some(i))), default)
          case _ =>
        }
      }

      if(isSortAny(symbol, None)) Sort.Any else
      class2sort.get(port2class((symbol, None))) match {
        case None => class2sort = class2sort.updated(port2class((symbol, None)), default)
        case _ =>
      }
    }

    // typing for each symbol
    (
      Signature(symbol2arity.map { case (symbol, (arity, theory, variadic)) =>
        val inputSorts = (0 until arity).toList.map { i =>
          if (isSortAny(symbol, Some(i))) Sort.Any
          else class2sort.get(port2class((symbol, Some(i)))) match {
            case Some(sort) => sort
            case None => throw new RuntimeException(s"Failed to derive the type of $symbol argument $i.")
          }
        }

        val outputSort = if (isSortAny(symbol, None)) Sort.Any else
          class2sort.get(port2class((symbol, None))) match {
            case Some(sort) => sort
            case None => throw new RuntimeException(s"Failed to derive the output sort of $symbol.")
          }

        FunctionSymbol(symbol, Typing(inputSorts, outputSort, theory, variadic), signature.left.find(_.name == symbol).flatMap(_.infix))
      }.toSet),
      variables.map { case port@((rule, name), _) => (rule, name) -> class2sort(port2class(port)) }.toMap
    )
  }

  def toSystem: System = {
    applyChains.toSystemIntroduceSorts
  }

  private def applyChains: QuasiSystem = {
    this.copy(rules = rules.map{ rule =>
      QuasiRule(applyChains(rule.left, chains), applyChains(rule.right, chains), rule.constraint.map(applyChains(_,chains)))
    })
  }

  private def applyChains(term: QuasiTerm, chains: Set[QuasiRule]): QuasiTerm = {
    chains.foldRight(term) { case (chain,t) => applyChain(t,chain) }
  }

  private def applyChain(term: QuasiTerm, chain: QuasiRule): QuasiTerm = term match {
    case App(f,args) =>
      val termArgsRewritten = App(f,args.map(applyChain(_,chain)))
      termArgsRewritten.instanceOf(chain.left) match {
        case Some(subsitution) => chain.right.substitute(subsitution)
        case None => termArgsRewritten
      }
  }

  private def toSystemIntroduceSorts: System = {
    val (signature, variableSortsAllRules) = deriveTypings
    val signatureMap = signature.asMap
    System(theory, logic, solver, signature, rules = rules.map { rule =>
      val variableSorts = variableSortsAllRules.filter(_._1._1 == rule).map { case ((_, name), sort) => name -> sort }
      rule.toRule(signatureMap, variableSorts)
    })
  }

  override def toString: String = {
    s"""SIGNATURE:
       |  ${signature.functions.map(_.toString).toList.sorted.mkString("\n  ")}
       |
       |RULES:
       |  ${rules.mkString("\n  ")}
       |""".stripMargin
  }
}
