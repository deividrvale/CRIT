package equiv.trs.parsing

import equiv.trs.Renaming

case class QuasiTheory(signature: QuasiSignature, renaming: Set[Renaming]) {
  def union(other: QuasiTheory): QuasiTheory = QuasiTheory(signature.union(other.signature), renaming ++ other.renaming)
}
