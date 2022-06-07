package equiv

import equiv.trs.parsing.TRSParser

import scala.io.Source

object TRSParserTest {
  def main(args: Array[String]): Unit = {
    new TRSParser(readFile).parseSystem(readFile("examples/decompose.ctrs")) match {
      case Left(quasiSystem) =>
        println(quasiSystem)
        val system = quasiSystem.toSystem
        println(system)
      case Right(error) => println(error.message)
    }
  }

  def readFile(file: String): String = {
    Source.fromResource(file).getLines().mkString("\n")
  }
}
