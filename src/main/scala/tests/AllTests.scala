package tests

object AllTests {
  def main(args: Array[String]): Unit = {
    EquivTest.main()
    TermUtilsTests.main()
    Unification.main()
  }
}
