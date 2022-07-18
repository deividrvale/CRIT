package equiv.utils

object ListExtension {
  extension [A](list: List[A]) {
    /** Applies a function [[f]] to this list if it is not empty.
     * @return [[Some]] result after applying [[f]] to [[this]] if [[this]] is not empty, [[None]] otherwise */
    def onNonEmpty[B](f: List[A] => Option[B]): Option[B] = {
      if list.isEmpty then None else f(list)
    }
  }
}
