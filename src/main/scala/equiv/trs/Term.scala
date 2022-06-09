package equiv.trs

trait Term {
  def sort: Sort
}

object Term {
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
