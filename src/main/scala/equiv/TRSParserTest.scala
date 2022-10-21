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
        val lctrs = parseTRS("examples/" + file.getName)
        lctrs match {
          case Some(system) =>
            println("OK")
          case None =>
            println("FAILED")
            failures += 1
        }
        println()
      }
    }
    println(s"Number of failures: $failures")
  }

  /** Tries to parse the .ctrs file with the given fileName. 
    * @return Some(System) on a success, None otherwise */
  def parseTRS(fileName: String) : Option[System] = {
    var parseResult : QuasiSystem = null
    try {
      new TRSParser(readFile).parseSystem(readFile(fileName)) match {
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
        e.printStackTrace()
        None
    }
  }

  def readFile(file: String): String = {
    Source.fromResource(file).getLines().mkString("\n")
  }
}
