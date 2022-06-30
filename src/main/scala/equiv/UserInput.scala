package equiv

abstract class UserInput[+A] { }

case class Input[+A](value: A) extends UserInput[A] {
  def get: A = value
}

case object Return extends UserInput[Nothing] { }

case object Auto extends UserInput[Nothing] { }

