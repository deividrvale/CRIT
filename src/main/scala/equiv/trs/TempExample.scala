package equiv.trs

val functionSymbol = FunctionSymbol("f", Typing(List(Sort.Int), Sort.Int))           // f : Int => Int
val varX = Var("x", Sort.Int)                                                        // x : Int
val minus = FunctionSymbol("-", Typing(List(Sort.Int, Sort.Int), Sort.Int, true))    // - : Int x Int => Int
val valZero = App(FunctionSymbol("0", Typing(List(), Sort.Int, true)), List())       // 0
val valOne = App(FunctionSymbol("1", Typing(List(), Sort.Int, true)), List())        // 1
val xMinusOne = App(minus, List(varX, valOne))                                       // x - 1
val funcGT = FunctionSymbol(">", Typing(List(Sort.Int, Sort.Int), Sort.Bool, true))  // > : Int x Int => Bool
val funcLE = FunctionSymbol("<=", Typing(List(Sort.Int, Sort.Int), Sort.Bool, true)) // <= : Int x Int => Bool
val funcReturn = FunctionSymbol("return", Typing(List(Sort.Int), Sort.Int))          // return : Int => Int

val l = App(functionSymbol, List(varX))
val r1 = App(functionSymbol, List(xMinusOne))
val r2 = App(funcReturn, List(valZero))
var c1 = Constraint(App(funcGT,List(varX, valZero)))
var c2 = Constraint(App(funcLE, List(varX, valZero)))

val rho1 = Rule(l, r1, Some(c1))  // f(x) -> f(x - 1)  [x > 0]
val rho2 = Rule(l, r2, Some(c2))  // f(x) -> return(0) [x <= 0]
