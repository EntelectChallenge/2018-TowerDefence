package za.co.sample

import java.io.FileWriter

import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._
import za.co.sample.models.Models.State


object Parser{
  implicit val formats = DefaultFormats

  def readFile(fileName: String): String = {
    Source.fromFile(fileName)("UTF-8").getLines().mkString
  }

  def readJsonParse4s(fileName: String): State = {
    val json = parse(fileName)
    json.extract[State]
  }

  def writeToFile(fileName:String, data:String): Unit =
    using (new FileWriter(fileName)) {
      fileWriter => fileWriter.write(data)
    }

  /**
    * Used for reading/writing to database, files, etc.
    * Code From the book "Beginning Scala"
    * http://www.amazon.com/Beginning-Scala-David-Pollak/dp/1430219890
    */
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }
}
