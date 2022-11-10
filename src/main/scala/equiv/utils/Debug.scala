package equiv.utils

object Debug {
  val doDebug = true

  def debug(message: String): Unit = {
    if doDebug then
      println(message)
  }
}
