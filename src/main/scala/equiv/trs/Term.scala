package equiv.trs

import equiv.ri.ProofState
import equiv.trs.Term.{App, Position, Substitution, Var}
import equiv.utils.{MapUtils, PrintUtils, TermUtils}

import scala.annotation.tailrec

trait Term {
  def sort: Sort

  def rootFunc: Option[FunctionSymbol]

  def isTheory: Boolean

  def isValue: Boolean

  /** Check if this term is basic, i.e. its root is a defined symbol and all its arguments are constructor terms */
  def isBasic(definedSymbols: Set[FunctionSymbol]): Boolean = this match {
    case Var(_, _) => false
    case App(f, args) => definedSymbols.contains(f) && args.forall(_.isConstructorTerm(definedSymbols))
  }

  /** Check if a term is a constructor term, i.e. it is in Terms(Cons, V), where Cons is the set of constructors. */
  def isConstructorTerm(definedSymbols: Set[FunctionSymbol]): Boolean = this match {
    case Var(_, _) => true
    case App(f, args) => f.isConstructor(definedSymbols) && args.forall(_.isConstructorTerm(definedSymbols))
  }

  /** Check if the given term is a calculation containing variables, so: a term in `Sigma_theory(Var)` that is not ground. */
  def isCalculationContainingVariables(isRoot: Boolean = true): Boolean = {
    this match {
      case Var(_, _) => !isRoot
      case App(f, args) => f.isTheory && (args.nonEmpty || !isRoot) && args.forall(_.isCalculationContainingVariables(false))
    }
  }

  /** @return A set of variables that occur in the given term */
  def vars: Set[Var] = this match {
    case x@Var(_, _) => Set(x)
    case App(_, args) => args.toSet.flatMap(_.vars)
  }

  /** @return A set of all occurring function symbols */
  def functionSymbols: Set[FunctionSymbol] = this match {
    case Var(_, _) => Set()
    case App(f, args) => Set(f) ++ args.flatMap(_.functionSymbols)
  }

  /** @return A set of the names of all occurring function symbols */
  def functionSymbolNames: Set[String] = this.functionSymbols.map(f => f.name)

  /** @return The subterm at the given position */
  def subTermAt(position: Position): Term = position match {
    case List() => this
    case x::xs => this match {
      case App(_, args) => args(x).subTermAt(xs)
    }
  }

  /** Checks if the current term (`this`) matches the given term.
   * @return A substitution if the terms match, otherwise None. */
  def instanceOf(other: Term) : Option[Substitution] = {
    if(this.sort != other.sort) None else
    (this,other) match {
      case (_, v@Var(_,_)) => Some(Map(v -> this)) // TODO check there is no other v in the substitution
      case (Var(_, _), App(_, _)) => None
      case (App(f1, args1), App(f2, args2)) =>
        if(f1 == f2) {
          (args1 zip args2).map(_.instanceOf(_)).foldLeft[Option[Substitution]](Some(Map.empty)){
            case (Some(map1),Some(map2)) => MapUtils.union(map1,map2)
            case _ => None
          }
        } else None
    }
  }

  /** Check if the current term (t1) is unifiable with the given term (t2), i.e. there exists a substitution γ such that t1γ = t2γ
   * TODO check Martelli-Montanari */
  def unifiableWith(term: Term): Option[Substitution] = { // TODO check correctness
    (this, term) match {
      case (v1@Var(x1, _), v2@Var(x2, _)) => if x1 == x2 then Some(Map()) else Some(Map(v2 -> v1))
      case (a@App(_, _), v@Var(_,_)) => Some(Map(v -> a))
      case (v@Var(_,_), a@App(_, _)) => if a.vars.contains(v) then None else Some(Map(v -> a))
      case (App(f1, args1), App(f2, args2)) =>
        if (f1 == f2) {
          (args1 zip args2).map(ts => ts._1.unifiableWith(ts._2)).foldLeft[Option[Substitution]](Some(Map.empty))({
            case (Some(map1), Some(map2)) => MapUtils.union(map1, map2)
            case _ => None
          })
        } else None
    }
  }

  /** Searches all subterms with the given property.
   * @param property A property of terms.
   * @return All subterms with the property together with their positions. */
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

  /** Searches all subterms that are instances of the given term. */
  def findSubTermInstances(other: Term): List[(Term, Position, Substitution)] = findSubTerms(t => t.instanceOf(other))

  /** @return If this is a (variadic) equality, return all variables in the term that are the child of an equality symbol, in a List.
   * @example [[getEqualityVars]] on term `((x = 5) = 1 + y) = z)` returns `List(z, x)` */
  def getEqualityVars: List[Term.Var] = {
    this match {
      case App(FunctionSymbol(TermUtils.equalityFunctionSymbolName, _, _, _, _, _), List(l, r)) =>
        (l, r) match {
          case (v@Var(_, _), App(_, _)) => List(v)
          case (App(_, _), v@Var(_, _)) => List(v)
          case _ => l.getEqualityVars ++ r.getEqualityVars
        }
      case _ => List()
    }
  }

  def maybeFindAssignment(term: Term): Option[Var] = {
    None
  }

  /** Substitute a subterm with `replacement` at the given `position`.
   * @param position Position of substitution as a list of Ints
   * @param replacement Term to substitute */
  def substituteAtPos(position: Position, replacement: Term): Term = (this, position, replacement) match {
    case (_, List(), _) => replacement
    case (App(f, args), x::xs, t2) => App(f, args.updated(x, args(x).substituteAtPos(xs, t2)))
    case _ => throw RuntimeException(s"Could not substitute position $position in $this by $replacement.")
  }

  /** Substitute all occurrences of `matchTerm` by `replacementTerm`
   * @param matchTerm Sub-term that will be replaced
   * @param replacementTerm Term that will replace occurrences of `matchTerm` */
  def substituteAll(matchTerm: Term, replacementTerm: Term): Term = {
    if this == matchTerm then replacementTerm else
    this match {
      case Var(_, _) => this
      case App(f, args) => App(f, args.map(_.substituteAll(matchTerm, replacementTerm)))
    }
  }

  /** Apply a substitution to all variables in a term
   * @return The term after the substitution */
  def applySubstitution(substitution: Substitution): Term = this match {
    case x@Var(_, _) => if substitution.contains(x) then substitution(x) else x
    case App(f, args) => App(f, args.map(_.applySubstitution(substitution)))
  }

  def rewriteAtPos(position: Position, rule: Rule, substitution: Substitution): Term =
    substituteAtPos(position, rule.right.applySubstitution(substitution))

  /** Check if the term is subject to the EQ-DELETION rule, i.e. it is in `Terms(Σ_theory, Var(ϕ))`, where ϕ is the constraint of the equation subject to EQ-DELETION.
   * @param constraintVars A set of variables (`Var(ϕ)`) from the constraint subject to the EQ-DELETION rule.
   * @return [[true]] if the term is subject to the EQ-DELETION rule, [[false]] otherwise. */
  def isEqDeletable(constraintVars: Set[Var]) : Boolean = this match {
    case v@Var(_, _) => constraintVars.contains(v)
    case App(f, args) => f.isTheory && args.forall(_.isEqDeletable(constraintVars))
  }

  def toStringApplicative : String = this match {
    case App(f,args) => if(args.isEmpty) s"${f.name}" else s"(${f.name} ${args.map(_.toStringApplicative).mkString(" ")})"
    case Var(v,_) => v
  }

  def toPrintString(colours: Boolean = true): String
}

object Term {
  type Position = List[Int]
  type Substitution = Map[Var, Term]

  def positionToString(p: Position): String = PrintUtils.positionColour ++ { if p.isEmpty then "root" else p.mkString("[", ":", "]") } ++ Console.RESET

  case class Var(name: String, sort: Sort) extends Term {
    override def rootFunc: Option[FunctionSymbol] = None

    override def isTheory: Boolean = sort.isTheory

    override def isValue: Boolean = false

    override def toString: String = toPrintString(false)

    override def toPrintString(colours: Boolean = true): String = {
      if colours then
        PrintUtils.variableColour + s"$name" + Console.RESET
      else
        s"$name"
    }
  }

  case class App(fun: FunctionSymbol, args: List[Term] = List.empty) extends Term {
    private val sortsArgsAny = args.indices.filter{ i => fun.typing.getSort(Some(i)).contains(Sort.Any) }.map{ i => args(i).sort }.toSet

    assert(
      args.length >= fun.typing.input.length &&
      args.indices.forall{ i => val sort = fun.typing.getSort(Some(i)); sort.contains(Sort.Any) || sort.contains(args(i).sort) } &&
      sortsArgsAny.size <= 1,
      s"The term ${fun.toPrintString()}(${args.map(_.toPrintString()).mkString(", ")}) is not well-typed; here $fun."
    )

    override def rootFunc: Option[FunctionSymbol] = Some(fun)

    override def isTheory: Boolean = fun.isTheory && args.forall(_.isTheory)

    override def isValue: Boolean = fun.isValue

    val sort: Sort = if(fun.typing.output == Sort.Any) sortsArgsAny.head else fun.typing.output

    override def toString: String = toPrintString(false)

    override def toPrintString(colours: Boolean = true): String = {
      if isInfix then
        s"${args.head.toPrintString(colours)} ${fun.toPrintString(colours)} ${args(1).toPrintString(colours)}"
        else
          s"${fun.toPrintString(colours)}${if(args.nonEmpty) args.map(_.toPrintString(colours)).mkString("(", ", ", ")") else ""}"
    }
    
    def isInfix: Boolean = this.fun.infix match {
      case None => false
      case Some(Infix(_, _)) => this.args.length == 2
    }
  }
}
