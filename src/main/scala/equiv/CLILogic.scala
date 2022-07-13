package equiv

import equiv.ri.Equation.Side
import equiv.ri.{Equation, ProofState}
import equiv.ri.inference_rules.{COMPLETENESS, CONSTRUCTOR, DELETION, DISPROVE, EQ_DELETION, EXPANSION, GENERALIZATION, POSTULATE, SIMPLIFICATION}
import equiv.trs.{Rule, Term}
import equiv.trs.Term.{Position, Substitution}
import equiv.utils.{Auto, Input, Return, TermUtils, UserInput, Z3}
import equiv.utils.OptionExtension.printFailureOnNone

import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap


class CLILogic(var pfSt: ProofState) {
  val stringAfterCorrectUserInput: String = "\n" * 5

  var forceQuit: Boolean = false
  val autoId: String = "-1"
  val returnId: String = "q"

  val c: Option[Int] = None

  val autoValues: List[String] = List("-1", "a", "au", "aut", "auto")
  val returnValues: List[String] = List("q", "r", "quit", "return", "ret", "exit", "e", "cancel")
  val leftValues: List[String] = List("0", "l", "left", "le", "lef", "links")
  val rightValues: List[String] = List("1", "r", "right", "ri", "rig", "righ", "rechts")
  assert(leftValues.intersect(rightValues).isEmpty)
  val noValues: List[String] = List("0", "n", "no", "nee")
  val yesValues: List[String] = List("1", "y", "ye", "yes", "ja")
  assert(noValues.intersect(yesValues).isEmpty)

  val actions: ListMap[String, (String, () => Unit)] = ListMap (
    returnId -> ("QUIT", () => forceQuit = true),
    autoId -> ("AUTO", () => println("Not implemented yet.")),
    "0" -> ("Simplify with calc", () => simplify_calc()),
    "1" -> ("DELETION", () => deletion()),
    "2" -> ("CONSTRUCTOR", () => constructor()),
    "3" -> ("EQ-DELETION", () => eq_deletion()),
    "4" -> ("SIMPLIFICATION", () => simplification()),
    "5" -> ("EXPANSION", () => expansion()),
    "6" -> ("POSTULATE", () => postulate()),
    "7" -> ("GENERALIZE", () => generalize()),
    "8" -> ("COMPLETENESS", () => completeness()),
    "9" -> ("DISPROVE", () => disprove()),
  )

  def RI(): Unit = {
    println(s"Starting Rewriting Induction with proofstate.")

    while
      !pfSt.isFinished && !forceQuit
    do {
      println(s"\n${pfSt.toPrintString()}\n")

      println(s"${Console.UNDERLINED}Choose action id${Console.RESET}: ")
      printOptions(actions.toList.map((nr, nameAction) => (nr, nameAction._1)).drop(2))
      var input = loopForCorrectInput(List(actions.keys.toList))
      input = handleDefaultUserInput(input, () => Input(input)) match {
        case Return => returnId
        case Auto => autoId
        case Input(id) => id
      }
      println(actions(input)._1)
      actions(input)._2()
    }
    println(s"Rewriting Induction terminated. Reason: " +
      s"${if forceQuit then s"${Console.RED}force quit"
      else if pfSt.isFinished then s"${Console.GREEN}no more equations"
      else if pfSt.isFalse then s"${Console.RED}proofstate is disproven"
      else "unknown"}${Console.RESET}.")
  }

  /** Prompt the user to choose an equation from the current proofstate.
   * @return `Input(Equation)` if an equation was selected or `None` if ''quit'' was selected. */
  def chooseEquation(): UserInput[Equation] = {
    chooseFromSet(pfSt.equations, "equation", _.toPrintString())
  }

  /** Prompt the user to choose a rule from the current proofstate.
   * @return `Input(Rule)` if a rule was selected or `None` if ''quit'' was selected. */
  def chooseRule() : UserInput[Rule] = {
    chooseFromSet(pfSt.rules ++ pfSt.hypotheses, "rule", _.toPrintString())
  }

  /** Prompt the user to choose an element from the current proofstate.
   * @return `Input(T)` if an element of type `T` was selected or `None` if ''quit'' was selected. */
  def chooseFromSet[T](set: Set[T], name: String, toPrintString: T => String): UserInput[T] = {
    val setElementsAndIndices = set.toSeq.zipWithIndex.map((elem, id) => (id.toString, elem))
    val setElementsAndIndicesMap = setElementsAndIndices.toMap
    println(s"${Console.UNDERLINED}Choose a${if "aeiou".contains(name(0)) then "n" else ""} $name${Console.RESET}: ")
    printOptions(setElementsAndIndices.map((id, elem) => (id, toPrintString(elem))))
    val input = loopForCorrectInput(List(setElementsAndIndicesMap.keys.toList))
    handleDefaultUserInput(input, { () =>
      println(s"${name.capitalize} ${toPrintString(setElementsAndIndicesMap(input))}.") ; Input(setElementsAndIndicesMap(input))
    })
  }

  /** Prompt the user to choose a side of an equation.
   * @return `Input(Side.Left)` or `Input(Side.Right)` or Empty if the 'auto' UserInput was selected */
  def chooseSide(equation: Equation): UserInput[Side] = {
    println(s"Equation ${equation.toPrintString()}")
    println(s"${Console.UNDERLINED}Choose a side${Console.RESET}: ")
    printOptions(List(("l", s"${equation.getSide(Side.Left).toPrintString()}"), ("r", s"${equation.getSide(Side.Right).toPrintString()}")))
    val input = loopForCorrectInput(List(leftValues, rightValues))
    handleDefaultUserInput(input, { () =>
      if leftValues.contains(input) then { println("Left") ; Input(Side.Left) }
      else { println("Right") ; Input(Side.Right) }
    })
  }

  /** Prompt the user to choose a subterm of the given term.
   * @return The position of the subterm. */
  def chooseSubterm(term: Term): UserInput[Position] = {
    val subtermsAndPositionsAndIndices = term.findSubTermsBool(_ => true).zipWithIndex.map((data, id) => (id.toString, data)) // TODO Get only expandable subterms
    val subtermsAndPositionsAndIndicesMap = subtermsAndPositionsAndIndices.toMap
    println(s"Term ${term.toPrintString()}")
    println(s"${Console.UNDERLINED}Choose a subterm${Console.RESET}: ")
    printOptions(subtermsAndPositionsAndIndices.map( (id, data) => (id, s"${data._1.toPrintString()}") ))
    val input = loopForCorrectInput(List(subtermsAndPositionsAndIndicesMap.keys.toList))
    handleDefaultUserInput(input, { () =>
      Input(subtermsAndPositionsAndIndicesMap(input)._2)
    })
  }

  /** Prompt the user to choose a subterm to apply a given rule to.
   * @return `None` if there are no possible rewrite positions or the user selects `quitid`.
   *         `Auto` if the user selected ''auto'',
   *         otherwise `Input()` with the corresponding `Position` and `Substitution` */
  def chooseSubtermRedex(term: Term, equation: Equation, rule: Rule): UserInput[(Position, Substitution)] = {
    val positionsAndIndices = SIMPLIFICATION.getAllPossibleRewritePlacesData(term, equation, rule).zipWithIndex.map((data, id) => (id.toString, data))
    val positionsAndIndicesMap = positionsAndIndices.toMap
    if positionsAndIndices.isEmpty then { println("No possible rewrite positions found.") ; return Return }
    println(s"Term ${term.toPrintString()}")
    println(s"${Console.UNDERLINED}Choose a subterm${Console.RESET}: ")
    printOptions(positionsAndIndicesMap.map((id, data) => (id, data._1.toPrintString())))
    val input = loopForCorrectInput(List(positionsAndIndicesMap.keys.toList))
    handleDefaultUserInput(input, { () =>
      val output = positionsAndIndicesMap(input)
      println(s"Subterm ${output._1.toPrintString()}.")
      Input((output._2, output._3))
    })
  }

  def chooseSubtermPairs(equation: Equation): UserInput[Set[Position]] = {
    var positionsAndPairsAndIndices: Iterable[(String, (Position, (Term, Term)))] =
      EQ_DELETION.getAllPossibleEqDeletionPositions(equation.left, equation.right, equation.constraintVars)
        .zipWithIndex.map((data, id) => (id.toString, data))
    var positionsAndPairsAndIndicesMap: Map[String, (Position, (Term, Term))] =
      positionsAndPairsAndIndices.toMap

    var positions: Set[Position] = Set()

    var yesNo = yesValues.head
    while
      yesValues.contains(yesNo) && positionsAndPairsAndIndices.nonEmpty
    do {
      println(s"Equation ${equation.toPrintString()}")
      println(s"${Console.UNDERLINED}Choose a subterm pair${Console.RESET}: ")
      printOptions(positionsAndPairsAndIndicesMap.map((id, data) => (id, s"${data._1}: ${data._2._1.toPrintString()}  and  ${data._2._2.toPrintString()}.")))
      val input = loopForCorrectInput(List(positionsAndPairsAndIndicesMap.keys.toList))
      handleDefaultUserInput(input, () => Input(input)) match {
        case Return => return Return
        case Auto => return Auto
        case Input(choice) =>
          val output = positionsAndPairsAndIndicesMap(choice)
          println(s"Subterms ${output._2._1.toPrintString()} and ${output._2._2.toPrintString()}.")
          positions += output._1
          positionsAndPairsAndIndices = positionsAndPairsAndIndices.filter((_, data) => !TermUtils.isOnPathOf(data._1, output._1)) // Filter out all positions that would interfere with the chosen position
          positionsAndPairsAndIndicesMap = positionsAndPairsAndIndices.toMap
      }
      // Ask the user if they want to choose another subterm pair
      println(s"${Console.UNDERLINED}Do you want to select another subterm?${Console.RESET}:")
      printOptions(Seq(("y", "Yes"), ("n", "No")), false, false)
      yesNo = loopForCorrectInput(List(yesValues, noValues), allowAuto = false, allowReturn = false)
    }
    if positionsAndPairsAndIndices.isEmpty then println("No possible subterms.")
    Input(positions)
  }

  /** Prompt the user to choose whether to add the given rule to the hypotheses set or not.
   * @return `Input(true)` if the user chose ''yes'', `Input(false)` if the user chose ''no'', `None` if the user chose ''quit'' or ''auto'' */
  def chooseAddRule(rule: Rule): UserInput[Boolean] = {
    println(s"Rule generated: ${rule.toPrintString()}.")
    println(s"${Console.UNDERLINED}Do you want to add this rule?${Console.RESET}: ")
    printOptions(Seq(("y", "Yes"), ("n", "No")), false, false)
    val input = loopForCorrectInput(List(yesValues, noValues), false, false)
    handleDefaultUserInput(input, { () =>
      if noValues.contains(input) then Input(false)
      else Input(true)
    })
  }

  // ===================================================================================================================
  // ================================================ HELPER FUNCTIONS =================================================
  // ===================================================================================================================

  /** Handle the default cases of user input string: if it is a return value, return [[Return]], if it is an auto value, return [[Auto]], otherwise execute a given function. */
  def handleDefaultUserInput[T](input: String, getContent: () => UserInput[T]): UserInput[T] = {
    if returnValues.contains(input) then Return
    else if autoValues.contains(input) then Auto
    else getContent()
  }

  /** Print a given list of options for the user to choose from, along with the default options, which can be turned off using the `printCancel` and `printAuto` parameters. */
  def printOptions(options: Iterable[(String, String)], printCancel: Boolean = true, printAuto: Boolean = true): Unit = {
    if printCancel then println(s" $returnId: Cancel")
    if printAuto then println(s"$autoId: Auto-choose")
    options.foreach((id, data) => println(s" $id: $data"))
  }

  /** Read the user's input until the value is in one of the given lists or, if the `allowAuto` or `allowReturn` parameters are [[true]], in the `autoValues` or `quitValues` lists respectively. */
  def loopForCorrectInput(choices: List[List[String]], allowAuto: Boolean = true, allowReturn: Boolean = true): String = {
    var acceptedInputs: List[List[String]] = choices
    if allowAuto then acceptedInputs ::= autoValues
    if allowReturn then acceptedInputs ::= returnValues
    var input = readLine().trim.toLowerCase
    while
      !acceptedInputs.exists(_.contains(input))
    do {
      print("Input not recognized. Please enter a valid input: ")
      input = readLine().trim.toLowerCase
    }
    print(stringAfterCorrectUserInput)
    input
  }

  // ===================================================================================================================
  // =========================================== INFERENCE RULES =======================================================
  // ===================================================================================================================

  /** Ancillary method to handle [[UserInput]].
   * @param input The user input to handle.
   * @param onInput Function to execute on the user input, if there is [[Input]]
   * @param onAuto Function to execute if the user selected [[Auto]]
   * @return A new proofstate */
  def handleUserInput[T1, T2](input: UserInput[T1], onInput: T1 => Option[T2], onAuto: () => Option[T2]): Option[T2] = {
    input match {
      case Input(x) => onInput(x)
      case Auto => onAuto()
      case Return => None
    }
  }

  def simplify_calc(): Unit = {
    println("Removing redundant constraints...")
    val newEquations = pfSt.equations.map(_.simplifyCons())
    if newEquations == pfSt.equations then
      println("No redundant equations found.")
    else
      pfSt = pfSt.copy(equations = newEquations)

    println("Trying to simplify with Z3...")
    pfSt = pfSt.replaceAllEquationWith(pfSt.equations.map(Z3.simplifyEquation))
  }

  def deletion(): Unit = {
    handleUserInput(
      input   = chooseEquation(),
      onInput = eq => DELETION.tryDeletionOnEquation(eq, pfSt),
      onAuto  = () => DELETION.tryDeletion(pfSt)
    ).printFailureOnNone(DELETION.name).foreach(pfSt = _)
  }

  def constructor(): Unit = {
    handleUserInput(
      input = chooseEquation(),
      onInput = eq => CONSTRUCTOR.tryConstructorOnEquation(eq, pfSt),
      onAuto = () => CONSTRUCTOR.tryConstructor(pfSt)
    ).printFailureOnNone(CONSTRUCTOR.name).foreach(pfSt = _)
  }

  def eq_deletion(): Unit = {
    handleUserInput(
      input = chooseEquation(),
      onAuto = () => EQ_DELETION.tryEqDeletion(pfSt),
      onInput = eq =>
        handleUserInput(
          input = chooseSubtermPairs(eq),
          onInput = positions => EQ_DELETION.tryEqDeletionOnEquationOnPositions(eq, positions, pfSt),
          onAuto = () => EQ_DELETION.tryEqDeletionOnEquation(eq, pfSt)
        ),
    ).printFailureOnNone(EQ_DELETION.name).foreach(pfSt = _)
  }

  def simplification(): Unit = {
    handleUserInput(
      // Choose equation
      input = chooseEquation(),
      onAuto = () => SIMPLIFICATION.trySimplification(pfSt),
      onInput = eq =>
        // Choose equation side
        handleUserInput(
          input = chooseSide(eq),
          onAuto = () => SIMPLIFICATION.trySimplificationOnEquation(eq, pfSt),
          onInput = side =>
            // Choose rule
            handleUserInput(
              input = chooseRule(),
              onAuto = () => SIMPLIFICATION.trySimplificationOnEquationSide(eq, side, pfSt),
              onInput = rule =>
                // Choose position
                handleUserInput(
                  input = chooseSubtermRedex(eq.getSide(side), eq, rule),
                  onAuto = () => SIMPLIFICATION.trySimplificationOnEquationSideWithRule(eq, side, rule, pfSt),
                  onInput = (pos, sub) => Some(SIMPLIFICATION.doSimplificationOnEquationSideWithRuleAtPosition(eq, side, rule, pos, sub, pfSt))
                )
            )
        )
    ).printFailureOnNone(SIMPLIFICATION.name).foreach(pfSt = _)
  }

  def expansion(): Unit = {
    def handleChooseRule(rule: Rule): Boolean = {
      chooseAddRule(rule) match {
        case Return => false
        case Auto => false
        case Input(b) => b
      }
    }

    handleUserInput(
      // Choose equation
      input = chooseEquation(),
      onAuto = () => EXPANSION.tryExpansion(pfSt, handleChooseRule),
      onInput = eq =>
        // Choose side
        handleUserInput(
          input = chooseSide(eq),
          onAuto = () => EXPANSION.tryExpansionOnEquation(eq, pfSt, handleChooseRule),
          onInput = side =>
            // Choose subterm (position)
            handleUserInput(
              input = chooseSubterm(eq.getSide(side)),
              onAuto = () => EXPANSION.tryExpansionOnEquationSide(List(), eq, side, pfSt, handleChooseRule),
              onInput = position => EXPANSION.tryExpansionOnEquationSideSubterm(position, eq, side, pfSt, handleChooseRule)
            )
        )
    ).printFailureOnNone(EXPANSION.name).foreach(pfSt = _)
  }

  def postulate(): Unit = {
    println("ERROR: Not implemented yet.")
  }

  def generalize(): Unit = {
    println("ERROR: Not implemented yet.")
  }

  def completeness(): Unit = {
    COMPLETENESS.tryCompleteness(pfSt)
      .printFailureOnNone(COMPLETENESS.name)
      .foreach(pfSt = _)
  }

  def disprove(): Unit = {
    handleUserInput(
      input = chooseEquation(),
      onAuto = () => DISPROVE.tryDisprove(pfSt),
      onInput = eq => DISPROVE.tryDisproveOnEquation(eq, pfSt)
    ).printFailureOnNone(DISPROVE.name).foreach(pfSt.isFalse = _)
  }

}
