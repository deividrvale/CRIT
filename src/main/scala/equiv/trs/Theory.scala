package equiv.trs

case class Theory(signature: Signature, renaming: Set[Renaming]) {
  def union(other: Theory): Theory = Theory(signature.union(other.signature), renaming ++ other.renaming)
}
