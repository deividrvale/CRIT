package equiv.trs

enum InfixKind:
  case Left, Right, Chain

case class Infix(kind: InfixKind, bindingStrength: Int)
