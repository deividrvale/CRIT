package equiv.trs.parsing

trait QuasiQuery
case class QuasiQueryEquivalence(equation: QuasiRule) extends QuasiQuery
case class QuasiQueryUnknown(query: String) extends QuasiQuery
