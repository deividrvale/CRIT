package equiv.trs.parsing

import equiv.trs.parsing.QuasiTerm.{App, InfixChain}
import equiv.trs.*

trait QuasiTerm {
  // collect all infix operators in a QuasiTerm
  def infixOperators : Set[String] = {
    this match {
      case App(_, args) => args.flatMap(_.infixOperators).toSet
      case InfixChain(head, tail) => tail.map(_._1).toSet ++ head.infixOperators ++ tail.flatMap(_._2.infixOperators)
    }
  }

  // function symbols
  def functionSymbols : Set[(String,Int)] = {
    this match {
      case App(fun, args) => Set((fun,args.length)) ++ args.flatMap(_.functionSymbols)
      case InfixChain(head, tail) => (head :: tail.map(_._2)).flatMap(_.functionSymbols).toSet
    }
  }

  // transform all infix operators to function application
  def infix2app(signature: Map[String,FunctionSymbol]) : App = {
    this match {
      case App(fun, args) =>
        App(fun, args.map(_.infix2app(signature)))
      case InfixChain(head, Nil) =>
        head.infix2app(signature)
      case quasiInfix @ InfixChain(head, tail) =>
        // find the weakest infix operators
        val infixOperators = tail.map(_._1).toSet.map{ op => (op, signature(op).infix.map(_.bindingStrength).get) }
        val weakestBinding = infixOperators.map(_._2).min
        val weakest = infixOperators.filter(_._2 == weakestBinding).map(_._1)

        // lift the weakest operators to the top
        val lifted = quasiInfix.liftOps(weakest)
        // transform the subterms
        val liftedApp = InfixChain(lifted.head.infix2app(signature), lifted.tail.map{ case (op,term) => (op, term.infix2app(signature)) })

        // introduce apps from left, right or chain
        signature(weakest.head).infix match {
          case Some(Infix(InfixKind.Left,_)) => liftedApp.appFromLeft.asInstanceOf[App]
          case Some(Infix(InfixKind.Right,_)) => liftedApp.appFromRight.asInstanceOf[App]
          case Some(Infix(InfixKind.Chain,_)) => App(weakest.head, head :: tail.map(_._2))
          case _ => throw new RuntimeException(s"Symbol ${weakest.head} has not been declared infix, but is used as infix.")
        }
    }
  }

  def toTerm(signature: Map[String,FunctionSymbol], variableSorts: Map[String,Sort]) : Term = {
    this match {
      case App(fun, args) =>
        signature.get(fun) match {
          case Some(symbol) =>
            Term.App(symbol, args.map(_.toTerm(signature, variableSorts)))
          case None =>
            if(args.nonEmpty) {
              throw new RuntimeException(s"The type of symbol $fun is unknown.")
            } else {
              Term.Var(fun, variableSorts(fun))
            }
        }
      case _ => throw new RuntimeException("The conversion from QuasiTerm to Term is only allows after all QuasiInfix have been eliminated.")
    }
  }
}

object QuasiTerm {
  case class App(fun: String, args: List[QuasiTerm]) extends QuasiTerm {
    override def toString: String = {
      s"$fun${if(args.nonEmpty) args.mkString("( ", ", ", " )") else ""}"
    }
  }

  // a chain of terms interspersed with infix operators
  case class InfixChain(head: QuasiTerm, tail: List[(String, QuasiTerm)]) extends QuasiTerm {
    // lift the given operators to the 'root'
    def liftOps(ops: Set[String]): InfixChain = {
      tail.zipWithIndex.find { case ((op, _), _) => ops.contains(op) } match {
        case Some((_, index)) =>
          val (prefix, suffix) = tail.splitAt(index)
          val suffixLifted = InfixChain(suffix.head._2, suffix.tail).liftOps(ops)
          InfixChain( InfixChain(head, prefix),  (suffix.head._1, suffixLifted.head) :: suffixLifted.tail )
        case None =>
          InfixChain(this, List.empty)
      }
    }

    def appFromLeft : QuasiTerm = {
      if(tail.isEmpty) return head
      App(tail.head._1, List(head, InfixChain(tail.head._2,tail.tail).appFromLeft))
    }

    def appFromRight : QuasiTerm = {
      if(tail.isEmpty) return head
      App(tail.last._1, List(InfixChain(head,tail.init).appFromRight, tail.last._2))
    }
  }
}

