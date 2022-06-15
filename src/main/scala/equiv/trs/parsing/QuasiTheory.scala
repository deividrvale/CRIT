package equiv.trs.parsing

import equiv.trs.Renaming

case class QuasiTheory(signature: QuasiSignature, chains: Set[QuasiRule], renaming: Set[Renaming]) {
  def union(other: QuasiTheory): QuasiTheory = QuasiTheory(signature.union(other.signature), chains ++ other.chains, renaming ++ other.renaming)
}
