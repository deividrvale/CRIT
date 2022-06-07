package equiv.trs

import equiv.trs.Term.App

object Core {
  val boolTrue: Term = App(FunctionSymbol("true",Typing(List(),Sort.Bool, true)),List())
  val boolFalse: Term = App(FunctionSymbol("false",Typing(List(),Sort.Bool, true)),List())
}
