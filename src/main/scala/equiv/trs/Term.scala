package equiv.trs

import equiv.ri.ProofState
import equiv.trs.Term.{App, Position, Substitution, Var}
import equiv.utils.{MapUtils, PrintUtils, TermUtils, TheorySymbols}

import scala.annotation.tailrec

trait Term {
  def sort: Sort

  def maybeRootFunc: Option[FunctionSymbol]

  def isTheory: Boolean

  def isValue: Boolean

  def isVar: Boolean = this match {
    case Var(_, _) => true
    case _ => false
  }

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

  /** Check if the given term is a calculation containing variables, so: a term in `Sigma_theory(Var) \ Var` that is not ground. */
  def isCalculationContainingVariables: Boolean = this match {
    case Var(_, _) => false
    case _ =>
      val r = this.isCalculationContainingVariablesAux
      r._1 && r._2
  }

  /** Recursively check if the given term contains only theory symbols and at least one variable.
   * @return A tuple [[(Boolean, Boolean)]],
   *         where the first argument represents whether all encountered elements are theory symbols or variables,
   *         and the second argument represents whether there is at least one variable. */
  private def isCalculationContainingVariablesAux: (Boolean, Boolean) =
    this match {
      case Var(_, _) => (true, true)
      case App(f, args) =>
        val bools = args.map(_.isCalculationContainingVariablesAux)
        (f.isTheory && bools.forall(_._1), bools.exists(_._2))
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
      case (_, v@Var(_,_)) => Some(Map(v -> this))
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

  /** Based on the Matching Algorithm from Terese.
   * Check if the given term is an instance of [[this]].
   * @return If this is the case, return a [[Substitution]] such that [[other]] can be obtained from [[this]].
   * [[None]] otherwise. */
  def matches(other: Term): Option[Substitution] = {
    var equations: List[(Term, Term)] = List((this, other))
    var substitution: Substitution = Map()
    while equations.nonEmpty do
      equations.head match {
        case (App(f1, args1), App(f2, args2)) =>
          if f1 == f2 then
            equations = equations ++ (args1 zip args2)
          else return None
        case (App(_, _), Var(_, _)) =>
          return None
        case (x@Var(_, _), t) =>
          if substitution.contains(x) then
            if substitution(x) != t then
              return None
          else
              substitution = substitution.updated(x, t)
      }
      equations = equations.drop(1)
    Some(substitution)
  }


  /** Based on the Martelli-Montanari unification algorithm.
   * @return [[None]] if the given [[Term]] is not unifiable with [[this]]. Otherwise returns [[Some]]([[Substitution]]) that unifies the two terms. */
  def unifiableWith(other: Term): Option[Substitution] = {
    var equations: List[(Term, Term)] = List((this, other))
    var substitution: Substitution = Map()
    while equations.nonEmpty do
      // pop the head of the list
      val head = equations.head
      equations = equations.drop(1)
      head match {
        case (a@App(_, _), v@Var(_, _)) =>
          equations = (v, a) :: equations
        case (v@Var(_, _), t) =>
          if v != t then // if we have {x = x} then ignore (x=x is already removed from the list)
            if t.vars.contains(v) then // if x ∈ Var(t), then fail
              return None
            else // if x not in Var(t)
              substitution = TermUtils.replaceVarInSub(v, t, substitution).updated(v, t)
              equations = TermUtils.replaceVarInTermPairs(v, t, equations)
        case (App(f1, args1), App(f2, args2)) =>
          if f1 != f2 then
            return None
          else equations = equations ++ (args1 zip args2)
      }
    Some(substitution)
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

  /** @return A set of all variables in the term that are the child of an equality symbol.
   * @example [[getEqualityVars]] on term `x = 5 = 1 + y = z` returns `Set(z, x)`. */
    def getEqualityVars: Set[Term.Var] = {
    this match {
      case App(FunctionSymbol(TermUtils.equalityFunctionSymbolName, _, _, _, _, _), args) => TermUtils.filterVars(args).toSet
      case _ => Set()
    }
  }

  /**
   * If [[this]] is an assignment containing the given term, return a set of all variables (directly) assigned to that term.
   * @param term The term to look for in the assignment
   * @return A set of all variables (directly) assigned to [[term]], that are not equal to [[term]].
   */
  def getVarsAssignedToTerm(term: Term): Set[Var] = {
    this match {
      case App(FunctionSymbol(TermUtils.equalityFunctionSymbolName, _, _, _, _, _), args) =>
        if args.contains(term) then
          TermUtils.filterVars(args).filter(_ != term).toSet
        else Set()
      case _ => Set()
    }
  }

  /**
   * If [[this]] term is an equality (so the root symbol is `=`), then return the terms that are assigned to the given variable.
   * @param v A variable
   * @return A set of terms that equal the given variable.
   */
  def getTermsAssignedToVar(v: Var): Set[Term] = {
    this match {
      case App(FunctionSymbol(TermUtils.equalityFunctionSymbolName, _, _, _, _, _), args) =>
        if args.contains(v) then
          args.filter(_ != v).toSet
        else Set()
      case _ => Set()
    }
  }

  /**
   * Get the variables and values in the term, in the order of occurrence when reading the term from left to right.
   * @example `f(x, g(4, x))` gives `[x, 4, x]`.
   * @return A [[List]] of [[Term]]s.
   */
  def getVarsValsInOrder: List[Term] = this match {
    case a@App(_, args) => if a.isValue then List(a) else args.flatMap(_.getVarsValsInOrder)
    case v@Var(_, _) => List(v)
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

  /** Rename every occurrence of a variable from the given [[List]] into a fresh variable.
   * @param variables A [[List]] of variables that we wish to rename when encountered in the current term
   * @return [[this]] [[Term]] with every occurrence of a variable in the given list renamed to a fresh variable. */
  def renameVarOccurrences(variables: Set[Var]): Term = {
    var term = this
    for (v <- term.vars) do {
      if variables.contains(v) then
        val replacementVar = TermUtils.getFreshVar(v.sort)
        term = term.substituteAll(v, replacementVar)
    }
    term
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
    case App(f,args) => if(args.isEmpty) s"${f.name}" else s"(${TheorySymbols.z3Convert(f.name)} ${args.map(_.toStringApplicative).mkString(" ")})"
    case Var(v,_) => v
  }

  def toPrintString(colours: Boolean = true): String
}

object Term {
  type Position = List[Int]
  type Substitution = Map[Var, Term]

  def positionToString(p: Position): String = PrintUtils.positionColour ++ { if p.isEmpty then "root" else p.mkString("[", ":", "]") } ++ Console.RESET

  case class Var(name: String, sort: Sort) extends Term {
    override def maybeRootFunc: Option[FunctionSymbol] = None

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

    override def maybeRootFunc: Option[FunctionSymbol] = Some(fun)

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
