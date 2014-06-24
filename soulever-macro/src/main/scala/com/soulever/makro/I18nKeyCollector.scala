package com.soulever.makro

import com.typesafe.config.{Config, ConfigFactory}
import scala.util.Try
import scala.io.{BufferedSource, Source}
import java.io.{File, PrintWriter}

object I18nKeyCollector {
  var keyList:Set[(String, String)] = Set.empty

  def insert(replaceKey:String = "")(key:String) = {
    keyList = keyList + (key -> replaceKey)
  }

  private val errorList = Map(
    "not-equal" -> "value should be equal to []",
    "integer" -> "value should be an integer",
    "max" -> "value should be lesser than []",
    "min" -> "value should be greater than []",
    "non-empty" -> "value should not be empty",
    "regex" -> "value should match with []"
  ).withDefault(a => dotNotationToValue(a))

  private def dotNotationToValue(s:String, replaceKey:String = ""):String = {

    s match {
      case s if s.matches("(.)+\\[(.)+\\]") =>
        val strings = s.split("[\\[\\]]")
        s"${dotNotationToValue(strings(0), replaceKey)} ${errorList(strings(1))}"
      case s if s.matches("(.)+\\{(.)+\\}") =>
        val strings = s.split("[\\{\\}]")
        s"${strings(1)}"
      case s =>
        s.replace(replaceKey, "").split("\\.").map(_.capitalize).mkString(" ").trim()
    }
  }

  def print = if(Props.isPrintable) {
    keyList.toList.sortBy(identity).map(_ + "=").mkString("\n")
    Props.printableFile.foreach {
      fileName =>
        val f: File = new File(fileName)
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
        keyMap.toList.sortBy(_._1).foreach{case (key,value) => writer.write(s"$key=$value\n")}
        writer.close()
    }
  }
}

object Props {
  val props = ConfigFactory.load("soulever.properties")

  def printableFile = Try(props.getString("i18n.print.path")).toOption

  def isPrintable = Try(props.getBoolean("i18n.print.on")).getOrElse(false)
}
