package equiv.trs

trait Term {
  def sort: Sort
}

case class Var(name: String, sort: Sort) extends Term

case class App(fun: FunctionSymbol, args: List[Term]) extends Term {
  assert(fun.typing.input == args.map(_.sort))

  def sort: Sort = fun.typing.output
}
