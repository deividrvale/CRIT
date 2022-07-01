package equiv

import equiv.ri.Equation.Side
import equiv.ri.{Equation, ProofState}
import equiv.ri.tactics.{COMPLETENESS, CONSTRUCTOR, DELETION, DISPROVE, EQ_DELETION, EXPANSION, GENERALIZATION, POSTULATE, SIMPLIFICATION}
import equiv.trs.{Rule, Term}
import equiv.trs.Term.{Position, Substitution}
import equiv.utils.{Auto, Input, Return, UserInput}
import equiv.utils.OptionExtension.printOnNone

import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap


class CLILogic(var pfSt: ProofState) {
  val stringAfterCorrectUserInput: String = "\n" * 5

  var forceQuit: Boolean = false
  val autoId: String = "-1"
  val quitId: String = "q"

  val c: Option[Int] = None

  val leftValues: List[String] = List("0", "l", "left", "le", "lef", "links")
  val rightValues: List[String] = List("1", "r", "right", "ri", "rig", "righ", "rechts")
  val autoValues: List[String] = List("2", "a", "au", "aut", "auto")
  assert(leftValues.intersect(rightValues).isEmpty)
  val noValues: List[String] = List("0", "n", "no", "nee")
  val yesValues: List[String] = List("1", "y", "ye", "yes", "ja")
  assert(noValues.intersect(yesValues).isEmpty)

  val actions: ListMap[String, (String, () => Unit)] = ListMap (
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
    quitId -> ("QUIT", () => forceQuit = true),
  )

  def RI(): Unit = {
    println(s"Starting Rewriting Induction with proofstate")

    while
      !pfSt.isFinished && !forceQuit
    do {
      println(s"\n${pfSt.toPrintString()}\n")

      actions.foreach((nr, nameAction) => println(s" $nr: ${nameAction._1}"))

      print(s"${Console.UNDERLINED}Choose action id${Console.RESET}: ")
      var input = readLine().trim.toLowerCase
      while !actions.contains(input)
      do {
        print("Action id not recognized. Enter a valid action id: ")
        input = readLine().toLowerCase
      }
      print(stringAfterCorrectUserInput)
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
    println(s"$autoId: Auto-choose")
    setElementsAndIndices.foreach((id, elem) => println(s" $id: ${toPrintString(elem)}"))
    println(s" $quitId: Return")
    print(s"${Console.UNDERLINED}Please choose a${if "aeiou".contains(name(0)) then "n" else ""} $name${Console.RESET}: ")
    var input = readLine().trim.toLowerCase
    while
      !(setElementsAndIndicesMap.contains(input) || input == autoId || input == quitId)
    do {
      print(s"${name.capitalize} id not recognized. Enter a valid id: ")
      input = readLine().trim.toLowerCase
    }
    print(stringAfterCorrectUserInput)
    if input == autoId then Auto
    else if input == quitId then Return
    else { println(s"${name.capitalize} ${toPrintString(setElementsAndIndicesMap(input))}.") ; Input(setElementsAndIndicesMap(input)) }
  }

  /** Prompt the user to choose a side of an equation.
   * @return `Input(Side.Left)` or `Input(Side.Right)` or Empty if the 'auto' UserInput was selected */
  def chooseSide(equation: Equation): UserInput[Side] = {
    println(s"$autoId: Auto-choose")
    println(s"l: ${equation.getSide(Side.Left).toPrintString()}")
    println(s"r: ${equation.getSide(Side.Right).toPrintString()}")
    println(s" $quitId: Return")
    print(s"${Console.UNDERLINED}Choose side${Console.RESET}: ")
    var input = readLine().trim.toLowerCase
    while
      !(leftValues.contains(input) || rightValues.contains(input) || autoValues.contains(input) || input == quitId)
    do {
      print("Side not recognized. Enter a valid side: ")
      input = readLine().trim.toLowerCase
    }
    print(stringAfterCorrectUserInput)
    if input == quitId then Return
    else if leftValues.contains(input) then { println("Left") ; Input(Side.Left) }
    else if rightValues.contains(input) then { println("Right") ; Input(Side.Right) }
    else Auto
  }

  /** Prompt the user to choose a subterm to apply a given rule to.
   * @return `None` if there are no possible rewrite positions or the user selects `quitid`.
   *         `Auto` if the user selected ''auto'',
   *         otherwise `Input()` with the corresponding `Position` and `Substitution` */
  def choosePosition(term: Term, equation: Equation, rule: Rule): UserInput[(Position, Substitution)] = {
    val positionsAndIndices = SIMPLIFICATION.getAllPossibleRewritePlacesData(term, equation, rule).zipWithIndex.map((data, id) => (id.toString, data))
    val positionsAndIndicesMap = positionsAndIndices.toMap
    if positionsAndIndices.isEmpty then { println("No possible rewrite positions found.") ; return Return }
    println(s"$autoId: Auto-choose")
    positionsAndIndicesMap.foreach((id, data) => println(s" $id: ${data._1.toPrintString()}"))
    println(s" $quitId: Return")
    print(s"${Console.UNDERLINED}Please choose a subterm to apply the rule to${Console.RESET}: ")
    var input = readLine().trim.toLowerCase
    while
      !(positionsAndIndicesMap.contains(input) || input == autoId || input == quitId)
    do {
      print(s"Subterm id not recognized. Enter a valid id: ")
      input = readLine().trim.toLowerCase
    }
    print(stringAfterCorrectUserInput)
    if input == autoId then Auto
    else if input == quitId then Return
    else {
      val output = positionsAndIndicesMap(input)
      println(s"Subterm ${output._1.toPrintString()}.")
      Input((output._2, output._3))
    }
  }

  /** Prompt the user to choose whether to add the given rule to the hypotheses set or not.
   * @return `Input(true)` if the user chose ''yes'', `Input(false)` if the user chose ''no'', `None` if the user chose ''quit'' or ''auto'' */
  def chooseAddRule(rule: Rule): UserInput[Boolean] = {
    println(s"Rule generated: ${rule.toPrintString()}")
    println(s" $quitId: Return")
    print(s"${Console.UNDERLINED}Do you want to add this rule? (y/n/auto)${Console.RESET}: ")
    var input = readLine().trim.toLowerCase
    while
      !(yesValues.contains(input) || noValues.contains(input) || autoValues.contains(input) || input == quitId)
    do {
      print("Answer not recognized. Please enter a valid answer (y/n/auto): ")
      input = readLine().trim.toLowerCase
    }
    print(stringAfterCorrectUserInput)
    if input == quitId then Return
    else if noValues.contains(input) then Input(false)
    else if yesValues.contains(input) then Input(true)
    else { println("Auto not implemented yet.") ; Auto } // TODO Check termination
  }

  // ===================================================================================================================
  // =========================================== INFERENCE RULES =======================================================
  // ===================================================================================================================

  /** Ancillary method to handle [[UserInput]].
   * @param input The user input to handle.
   * @param onInput Function to execute on the user input, if there is [[Input]]
   * @param onAuto Function to execute if the user selected [[Auto]]
   * @return A new proofstate */
  def handleUserInput[T](name: String, input: UserInput[T], onInput: T => Option[ProofState], onAuto: () => Option[ProofState]): Option[ProofState] = {
    input match {
      case Input(x) => onInput(x).printOnNone(s"$name failed")
      case Auto => onAuto().printOnNone(s"$name failed")
      case Return => None
    }
  }

  def simplify_calc(): Unit = {
    println("ERROR: Not implemented yet")
  }

  def deletion(): Unit = {
    handleUserInput(
      name = "DELETION",
      input   = chooseEquation(),
      onInput = eq => DELETION.tryDeletionOnEquation(eq, pfSt),
      onAuto  = () => DELETION.tryDeletion(pfSt)
    ).foreach(pfSt = _)
  }

  def constructor(): Unit = {
    handleUserInput(
      name = "CONSTRUCTOR",
      input = chooseEquation(),
      onInput = eq => CONSTRUCTOR.tryConstructorOnEquation(eq, pfSt),
      onAuto = () => CONSTRUCTOR.tryConstructor(pfSt)
    ).foreach(pfSt = _)
  }

  def eq_deletion(): Unit = {
    (chooseEquation() match {
      case Input(eq) => EQ_DELETION.tryEqDeletionOnEquation(eq, pfSt) match {
        case Some(newEq) => Some(pfSt.replaceEquationWith(eq, newEq))
        case None => println("EQ-DELETION failed"); None
      }
      case Auto => EQ_DELETION.tryEqDeletion(pfSt) match {
        case None => println("EQ-DELETION failed"); None
        case x => x
      }
      case Return => None
    }).foreach( newPfSt => pfSt = newPfSt )
  }

  def simplification(): Unit = {
    (chooseEquation() match {
      case Input(eq) => chooseSide(eq) match {
        case Input(side) => chooseRule() match {
          case Input(rule) => choosePosition(eq.getSide(side), eq, rule) match {
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
