package equiv.trs

import equiv.trs.Term.{App, Position, Var}
import equiv.utils.MapUtils

trait Term {
  def sort: Sort

  /** @return A list of variables that occur in the given term */
  def vars: Set[Var] = this match {
    case x@Var(_, _) => Set(x)
    case App(_, args) => args.toSet.flatMap(_.vars)
  }

  /**
   * Checks if the current term matches the given term.
   * @return A substitution if the terms match, otherwise None.
   */
  def instanceOf(other: Term) : Option[Map[Var,Term]] = {
    if(this.sort != other.sort) None else
    (this,other) match {
      case (_, v@Var(_,_)) => Some(Map(v -> this))
      case (Var(_, _), App(_, _)) => None
      case (App(f1, args1), App(f2, args2)) =>
        if(f1 == f2 && args1.length == args2.length) {
          (args1 zip args2).map(_.instanceOf(_)).foldLeft[Option[Map[Var,Term]]](Some(Map.empty)){
            case (Some(map1),Some(map2)) => MapUtils.union(map1,map2)
            case _ => None
          }
        } else None
    }
  }

  /**
   * Searches all subterms with the given property.
   * @param property A property of terms.
   * @return All subterms with the property together with their positions.
   */
  def findSubTerms[X](property: Term => Option[X]) : List[(Term,Position,X)] = {
    val result = this match {
      case Var(_, _) => List.empty
      case App(_, args) => args.zipWithIndex.flatMap{ case (arg,i) => arg.findSubTerms(property).map{ case (t,p,x) => (t,i::p,x) } }
    }
    property(this) match {
      case Some(x) => (this,List.empty,x) :: result
      case None => result
    }
  }

  def findSubTermsBool(property: Term => Boolean) : List[(Term,Position)] = {
    findSubTerms({ t => if(property(t)) Some(()) else None }).map{ case (t,p,_) => (t,p) }
  }

  /**
   * Searches all subterms that are instances of the given term.
   */
  def findSubTermInstances(other: Term): List[(Term, Position, Map[Var, Term])] = findSubTerms(t => t.instanceOf(other))

  /** Substitute a subterm with `replacement` at the given `position`.
   * @param position Position of substitution as a list of Ints
   * @param replacement Term to substitute
   */
  def substituteAtPos(position: List[Int], replacement: Term): Term = (this, position, replacement) match {
    case (_, List(), _) => replacement
    case (App(f, args), x::xs, t2) => App(f, args.updated(x, args(x).substituteAtPos(xs, t2)))
    case _ => throw RuntimeException(s"Could not substitute position ${position} in ${this} by ${replacement}.")
  }

  /** Substitute all occurrences of `matchTerm` by `replacementTerm`
   * @param matchTerm Sub-term that will be replaced
   * @param replacementTerm Term that will replace occurrences of `matchTerm`
   */
  def substituteAll(matchTerm: Term, replacementTerm: Term): Term = {
    if this == matchTerm then replacementTerm else
    this match {
      case Var(_, _) => this
      case App(f, args) => App(f, args.map(_.substituteAll(matchTerm, replacementTerm)))
    }
  }
}

object Term {
  type Position = List[Int]

  case class Var(name: String, sort: Sort) extends Term {
    override def toString: String = {
      s"$name" //s"$name:$sort"
    }
  }

  case class App(fun: FunctionSymbol, args: List[Term]) extends Term {
    private val sortsArgsAny: Set[Sort] = args.indices.filter{ i => fun.typing.getSort(Some(i)).contains(Sort.Any) }.map{ i => args(i).sort }.toSet

    assert(
      args.length >= fun.typing.input.length &&
      args.indices.forall{ i => val sort = fun.typing.getSort(Some(i)); sort.contains(Sort.Any) || sort.contains(args(i).sort) } &&
      sortsArgsAny.size <= 1,
      s"The term ${fun.name}( ${args.mkString(", ")} ) is not well-typed; here $fun."
    )

    val sort: Sort = if(fun.typing.output == Sort.Any) sortsArgsAny.head else fun.typing.output

    override def toString: String = {
      if isInfix then s"${args.head} ${fun.name} ${args(1)}" else
      s"${fun.name}${if(args.nonEmpty) args.mkString("(", ", ", ")") else ""}"
    }

    def isInfix: Boolean = this.fun.infix match {
        case None => false
        case Some(Infix(_, _)) => this.args.length == 2
    }
  }
}
