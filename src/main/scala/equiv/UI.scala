/***
 * // https://www.scalafx.org/api/8.0/#package
 */
package equiv

import equiv.ri.ProofState
import equiv.trs.Temp.InferenceRuleEquations.{constructorEquation, deletionEquation1, deletionEquation2, disproveEquation1, disproveEquation2, disproveEquation3, disproveEquation4, expansionEquation, rule1, rule2}
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.{Font, Text}
import equiv.trs.Temp.TestEquations

object UI extends JFXApp3 {
  override def start(): Unit = {

    val equations = Set(constructorEquation, deletionEquation1, deletionEquation2, disproveEquation1, disproveEquation2, disproveEquation3, disproveEquation4)
    val p1 = ProofState(Set(expansionEquation) ++ equations, Set(rule1, rule2))

    stage = new JFXApp3.PrimaryStage {
      title = "LCTRS RI Tool"
      scene = new Scene(500,500) {
        val equations = new Text(20,20,p1.equations.mkString(sep="\n"))
        equations.font = Font("Consolas", 15)

        val textField = new TextField()
        textField.layoutX = 20
        textField.layoutY = 420
        content = List(equations, textField)
      }
    }

  }

  def oldScene: Scene = new Scene(500,500) {
    val button: Button = new Button("A")
    button.layoutX = 300
    button.layoutY = 300
    val rect: Rectangle = Rectangle(10,10,200,200)
    rect.fill = Color.Aqua
    button.onMouseClicked = {
      _ => rect.fill = Color.Black
    }
    val text = new Text(300,400, "aa")
    content = List(button, rect, text)
  }
}