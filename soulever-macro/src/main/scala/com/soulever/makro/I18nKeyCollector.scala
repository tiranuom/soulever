package com.soulever.makro

import com.typesafe.config.{ConfigValue, Config, ConfigFactory}
import scala.util.Try
import scala.io.{BufferedSource, Source}
import java.io.{File, PrintWriter}
import java.util.Properties

class I18nKeyCollector(printableFile:Option[String]) {

  var props = Map.empty[String,String]

  val fileProps = new Properties()

  printableFile.foreach(name => Try(fileProps.load(Source.fromFile(new File(".").getAbsoluteFile + name).bufferedReader())) )

  var keyList:Set[(String, String)] = Set.empty

  def insert(replaceKey:String = "")(key:String) = {
    keyList = keyList + (key -> replaceKey)
    props = props + (key -> dotNotationToValue(key, replaceKey))
  }

  private val errorList = Map(
    "not-equal" -> "value should be equal to []",
    "integer" -> "value should be an integer",
    "long" -> "should be a long",
    "float" -> "should be a float value",
    "double" -> "should be a double value",
    "byte" -> "should be a byte value",
    "max" -> "value should be lesser than []",
    "min" -> "value should be greater than []",
    "non-empty" -> "value should not be empty",
    "regex" -> "value should match with []"
  ).withDefault(a => s"should be [${dotNotationToValue(a)}]")

  private def dotNotationToValue(s:String, replaceKey:String = ""):String = s match {
    case s if s.matches("(.)+\\[(.)+\\]") =>
      val strings = s.split("[\\[\\]]")
      s"${dotNotationToValue(strings(0), replaceKey)} ${errorList(strings(1))}"
    case s if s.matches("(.)+\\{(.)+\\}") =>
      val strings = s.split("[\\{\\}]")
      s"${strings(1)}"
    case s =>
      s.replace(replaceKey, "").split("\\.").map(_.capitalize).mkString(" ").trim()
  }

  def print = {
    keyList.toList.sortBy(identity).map(_ + "=").mkString("\n")
    printableFile.map(new File(".").getAbsoluteFile + _).foreach {
      fileName =>
        val f: File = new File(fileName)
        println("can write" + f.canWrite)
        val file: BufferedSource = Source.fromFile(f)
        var keyMap = keyList.map{ case (a, b) => a -> dotNotationToValue(a, b)}.toMap
        val lines: List[String] = file.getLines().toList
        file.close()
        val writer: PrintWriter = new PrintWriter(f)
        lines.foreach {
          entry =>
            val splits: Array[String] = entry.split("=")
            keyMap = keyMap + (splits(0) -> Try(splits(1)).getOrElse(""))
        }
        keyMap.toList.sortBy(_._1).foreach{
          case (key,value) =>
            println(s"$key=$value\n")
            writer.write(s"$key=$value\n")
        }

        writer.close()
    }
  }

  def i18n(s:String) = Try(fileProps.getProperty(s)).toOption.flatMap(Option.apply).getOrElse(props.withDefaultValue(s)(s))
}
