package equiv

import equiv.trs.System
import equiv.trs.parsing.{QuasiSystem, TRSParser}

import java.io.File
import scala.io.Source

object TRSParserTest {
  def main(args: Array[String]): Unit = {
    var failures = 0
    val path = getClass.getResource("/examples")
    val folder = new File(path.getPath)
    if (folder.exists && folder.isDirectory) {
      folder.listFiles.filter(_.isFile).filter(_.getName.endsWith(".ctrs")).toList.sorted.foreach{ file =>
        println(file.getName)
        println(if(parseTRS("examples/" + file.getName).isEmpty) { failures += 1; "FAILED" } else "ok")
        println()
      }
    }
    println(s"Number of failures: $failures")
  }

  def parseTRS(name: String) : Option[System] = {
    var parseResult : QuasiSystem = null
    try {
      new TRSParser(readFile).parseSystem(readFile(name)) match {
        case Left(quasiSystem) =>
          parseResult = quasiSystem
          Some(quasiSystem.toSystem)
        case Right(error) =>
          println(s"Parsing failed: ${error.message}")
          None
      }
    } catch {
      case e : Throwable =>
        if(parseResult != null) println("Parse result:\n" + parseResult.toString)
        println(s"Error: ${e.getMessage}")
        None
    }
  }

  def readFile(file: String): String = {
    Source.fromResource(file).getLines().mkString("\n")
  }
}
