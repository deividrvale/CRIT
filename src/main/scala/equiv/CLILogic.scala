package equiv

import equiv.ri.Equation.Side
import equiv.ri.{Equation, ProofState}
import equiv.ri.tactics.{COMPLETENESS, CONSTRUCTOR, DELETION, DISPROVE, EQ_DELETION, EXPANSION, GENERALIZATION, POSTULATE, SIMPLIFICATION}
import equiv.trs.{Rule, Term}
import equiv.trs.Term.{Position, Substitution}
import equiv.utils.{Auto, Input, Return, TermUtils, UserInput}
import equiv.utils.OptionExtension.printOnNone

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
    println(s"Starting Rewriting Induction with proofstate")

    while
      !pfSt.isFinished && !forceQuit
    do {
      println(s"\n${pfSt.toPrintString()}\n")

      printOptions(actions.toList.map((nr, nameAction) => (nr, nameAction._1)).drop(2))

      print(s"${Console.UNDERLINED}Choose action id${Console.RESET}: ")
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
      s"${if forceQuit then "force quit"
      else if pfSt.isFinished then "proofstate is in terminal state"
      else if pfSt.isFalse then "proofstate is disproven"
      else "unknown"}.")
  }

  def chooseEquation(): UserInput[Equation] = {
    chooseFromSet(pfSt.equations, "equation", _.toPrintString())
  }

  def chooseRule() : UserInput[Rule] = {
    chooseFromSet(pfSt.rules, "rule", _.toPrintString())
  }

  /** Prompt the user to choose an element from the current proofstate.
   * @return `Input(T)` if an element of type `T` was selected or `None` if ''quit'' was selected. */
  def chooseFromSet[T](set: Set[T], name: String, toPrintString: T => String): UserInput[T] = {
    val setElementsAndIndices = set.toSeq.zipWithIndex.map((elem, id) => (id.toString, elem))
    val setElementsAndIndicesMap = setElementsAndIndices.toMap
    printOptions(setElementsAndIndices.map((id, elem) => (id, toPrintString(elem))))
    print(s"${Console.UNDERLINED}Choose a${if "aeiou".contains(name(0)) then "n" else ""} $name${Console.RESET}: ")
    val input = loopForCorrectInput(List(setElementsAndIndicesMap.keys.toList))
    handleDefaultUserInput(input, { () =>
      println(s"${name.capitalize} ${toPrintString(setElementsAndIndicesMap(input))}.") ; Input(setElementsAndIndicesMap(input))
    })
  }

  /** Prompt the user to choose a side of an equation.
   * @return `Input(Side.Left)` or `Input(Side.Right)` or Empty if the 'auto' UserInput was selected */
  def chooseSide(equation: Equation): UserInput[Side] = {
    println(s"$autoId: Auto-choose")
    println(s"l: ${equation.getSide(Side.Left).toPrintString()}")
    println(s"r: ${equation.getSide(Side.Right).toPrintString()}")
    println(s" $returnId: Return")
    print(s"${Console.UNDERLINED}Choose a side${Console.RESET}: ")
    val input = loopForCorrectInput(List(leftValues, rightValues))
    handleDefaultUserInput(input, { () =>
      if leftValues.contains(input) then { println("Left") ; Input(Side.Left) }
      else { println("Right") ; Input(Side.Right) }
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
    printOptions(positionsAndIndicesMap.map((id, data) => (id, data._1.toPrintString())))
    print(s"${Console.UNDERLINED}Choose a subterm${Console.RESET}: ")
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
      printOptions(positionsAndPairsAndIndicesMap.map((id, data) => (id, s"${data._2._1.toPrintString()}  and  ${data._2._2.toPrintString()}")))
      print(s"${Console.UNDERLINED}Choose a subterm pair${Console.RESET}: ")
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
      printOptions(Seq(("y", "Yes"), ("n", "No")), false, false)
      print(s"${Console.UNDERLINED}Do you want to select another subterm?${Console.RESET}:")
      yesNo = loopForCorrectInput(List(yesValues, noValues), allowAuto = false, allowReturn = false)
    }
    if positionsAndPairsAndIndices.isEmpty then println("No possible subterms.")
    Input(positions)
  }

  /** Prompt the user to choose whether to add the given rule to the hypotheses set or not.
   * @return `Input(true)` if the user chose ''yes'', `Input(false)` if the user chose ''no'', `None` if the user chose ''quit'' or ''auto'' */
  def chooseAddRule(rule: Rule): UserInput[Boolean] = {
    println(s"Rule generated: ${rule.toPrintString()}")
    printOptions(Seq(("y", "Yes"), ("n", "No")))
    print(s"${Console.UNDERLINED}Do you want to add this rule?${Console.RESET}: ")
    val input = loopForCorrectInput(List(yesValues, noValues))
    handleDefaultUserInput(input, { () =>
      if noValues.contains(input) then Input(false)
      else Input(true)
    })
  }

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
  def handleUserInput[T](input: UserInput[T], onInput: T => Option[ProofState], onAuto: () => Option[ProofState]): Option[ProofState] = {
    input match {
      case Input(x) => onInput(x)
      case Auto => onAuto()
      case Return => None
    }
  }

  def simplify_calc(): Unit = {
    println("ERROR: Not implemented yet")
  }

  def deletion(): Unit = {
    handleUserInput(
      input   = chooseEquation(),
      onInput = eq => DELETION.tryDeletionOnEquation(eq, pfSt),
      onAuto  = () => DELETION.tryDeletion(pfSt)
    ).printOnNone(s"${DELETION.name} failed.").foreach(pfSt = _)
  }

  def constructor(): Unit = {
    handleUserInput(
      input = chooseEquation(),
      onInput = eq => CONSTRUCTOR.tryConstructorOnEquation(eq, pfSt),
      onAuto = () => CONSTRUCTOR.tryConstructor(pfSt)
    ).printOnNone(s"${CONSTRUCTOR.name} failed.").foreach(pfSt = _)
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
    ).printOnNone(s"${EQ_DELETION.name} failed.").foreach(pfSt = _)
  }

  def simplification(): Unit = {
    (chooseEquation() match {
      case Input(eq) => chooseSide(eq) match {
        case Input(side) => chooseRule() match {
          case Input(rule) => chooseSubtermRedex(eq.getSide(side), eq, rule) match {
            case Input((pos, sub)) => Some(pfSt.replaceEquationWith(eq, SIMPLIFICATION.doSimplificationOnEquationSideWithRuleAtPosition(eq, side, rule, pos, sub)))
            case Auto => SIMPLIFICATION.trySimplificationOnEquationSideWithRule(eq, side, rule).map(eq2 => pfSt.replaceEquationWith(eq, eq2))
            case Return => None
          }
          case Auto => SIMPLIFICATION.trySimplificationOnEquationSide(eq, side, pfSt.rules).map(eq2 => pfSt.replaceEquationWith(eq, eq2))
          case Return => None
        }
        case Auto => SIMPLIFICATION.trySimplificationOnEquation(eq, pfSt.rules).map(eq2 => pfSt.replaceEquationWith(eq, eq2))
        case Return => None
      }
      case Auto => SIMPLIFICATION.trySimplification(pfSt)
      case Return => None
    }).foreach(newPfSt => pfSt = newPfSt )
  }

  def expansion(): Unit = {
    (chooseEquation() match {
      case Input(eq) => chooseSide(eq) match {
        case Input(side) => EXPANSION.tryExpansionOnEquationSide(eq, side, pfSt.rules, pfSt) match {
          case Some((eqs, maybeRule)) => maybeRule match {
            case Some(rule) => chooseAddRule(rule) match {
              case Input(true) => Some(pfSt.removeEquation(eq).addEquations(eqs).addRule(rule))
              case Input(false) => Some(pfSt.removeEquation(eq).addEquations(eqs))
              case Auto => Some(pfSt.removeEquation(eq).addEquations(eqs))
              case Return => None
            }
            case None => Some(pfSt.removeEquation(eq).addEquations(eqs))
          }
          case None => println("No suitable rule found (not terminating)."); None
        }
        case Auto => EXPANSION.tryExpansionOnEquation(eq, pfSt.rules, pfSt) match {
          case Some((eqs, maybeRule)) => maybeRule match {
            case Some(rule) => chooseAddRule(rule) match {
              case Input(true) => Some(pfSt.removeEquation(eq).addEquations(eqs).addRule(rule))
              case Input(false) => Some(pfSt.removeEquation(eq).addEquations(eqs))
              case Auto => Some(pfSt.removeEquation(eq).addEquations(eqs))
              case Return => None
            }
            case None => Some(pfSt.removeEquation(eq).addEquations(eqs))
          }
          case None => None
        }
        case Return => None
      }
      case Auto => EXPANSION.tryExpansion2(pfSt) match {
        case Some((newPfSt, maybeRule)) => maybeRule match {
          case Some(rule) => chooseAddRule(rule) match {
            case Input(true) => Some(newPfSt.addRule(rule))
            case Input(false) => Some(newPfSt)
            case Auto => Some(newPfSt)
            case Return => None
          }
          case None => None
        }
        case None => None
      }
      case Return => None
    }).foreach(newPfSt => pfSt = newPfSt)
  }

  def postulate(): Unit = {
    println("ERROR: Not implemented yet")
  }

  def generalize(): Unit = {
    println("ERROR: Not implemented yet")
  }

  def completeness(): Unit = {
    COMPLETENESS.tryCompleteness(pfSt)
      .map(newPfSt => pfSt = newPfSt )
      .getOrElse( println("COMPLETENESS failed") )
  }

  def disprove(): Unit = {
    chooseEquation() match {
      case Input(eq) => DISPROVE.tryDisproveOnEquation(eq, pfSt) match {
        case Some(_) => pfSt.isFalse = true
        case None => println("DISPROVE failed")
      }
      case Auto => DISPROVE.tryDisprove(pfSt) match {
        case Some(_) => pfSt.isFalse = true
        case None => println("DISPROVE failed")
      }
      case Return =>
    }
  }

}
