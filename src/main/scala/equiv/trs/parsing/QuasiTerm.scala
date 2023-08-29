package equiv.trs.parsing

import equiv.trs.parsing.QuasiTerm.{App, InfixChain}
import equiv.trs.*
import equiv.utils.MapUtils
import sun.security.pkcs11.P11Util

trait QuasiTerm {
  // collect all infix operators in a QuasiTerm
  def infixOperators : Set[String] = {
    this match {
      case App(_, args) => args.flatMap(_.infixOperators).toSet
      case InfixChain(head, tail) => tail.map(_._1).toSet ++ head.infixOperators ++ tail.flatMap(_._2.infixOperators)
    }
  }

  /** Set of pairs of all function symbols occurring in the quasiterm, together with their number of arguments */
  def functionSymbols : Set[(String,Int)] = {
    this match {
      case App(fun, args) => Set((fun,args.length)) ++ args.flatMap(_.functionSymbols)
      case InfixChain(head, tail) => (head :: tail.map(_._2)).flatMap(_.functionSymbols).toSet
    }
  }

  /**
   * Tries to automatically determine the sorts of the variables.
   * !! Assumes [[this]] does not contain [[InfixChain]]s. Call method [[infix2app]] before this method if there are still [[InfixChain]]s.
   * @param signature The signature
   * @return Variablesorts
   */
  def getVariableSorts(signature: Map[String, FunctionSymbol]): Map[String, Sort] = {
    this match {
      case App(_, List()) => Map()
      case App(fun, args) =>
        args.zipWithIndex.flatMap( (quasiTerm, index) => quasiTerm match {
          case App(v, List()) => Map((v, signature(fun).typing.input(index) ))
          case App(_, args) => args.flatMap(_.getVariableSorts(signature))
        }).toMap
    }
  }

  /** Transform all infix operators to function application */
  def infix2app(signature: Map[String,FunctionSymbol]) : App = {
    this match {
      case App(fun, args) =>
        App(fun, args.map(_.infix2app(signature)))
      case InfixChain(head, List()) =>
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
          case Some(Infix(InfixKind.Chain,_)) => App(weakest.head, liftedApp.head :: liftedApp.tail.map(_._2))
          case _ => throw new RuntimeException(s"Symbol ${weakest.head} has not been declared infix, but is used as infix.")
        }
    }
  }

  /** Checks if the current term matches the given term.
   * @return A substitution if the terms match, otherwise None. */
  def instanceOf(other: QuasiTerm) : Option[Map[String,QuasiTerm]] = {
    (this,other) match {
      case (_, App(x,Nil)) => Some(Map(x -> this))
      case (App(f1, args1), App(f2, args2)) =>
        if(f1 == f2 && args1.length == args2.length) {
          (args1 zip args2).map(_.instanceOf(_)).foldLeft[Option[Map[String,QuasiTerm]]](Some(Map.empty)){
            case (Some(map1),Some(map2)) => MapUtils.union(map1,map2)
            case _ => None
          }
        } else None
    }
  }

  def substitute(s: Map[String,QuasiTerm]) : QuasiTerm = this match {
    case App(f, args) => if (s.contains(f) && args.isEmpty) s(f) else App(f, args.map(_.substitute(s)))
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
      case _ => throw new RuntimeException("The conversion from QuasiTerm to Term is only allowed after all QuasiInfix have been eliminated.")
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

