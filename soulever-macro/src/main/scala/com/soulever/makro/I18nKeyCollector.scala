package com.soulever.makro

import com.typesafe.config.{Config, ConfigFactory}
import scala.util.Try
import scala.io.{BufferedSource, Source}
import java.io.{File, PrintWriter}

object I18nKeyCollector {
  var keyList:Set[String] = Set.empty

  def insert(key:String) = {
    keyList = keyList + key
  }

  def print = if(Props.isPrintable) {
    keyList.toList.sortBy(identity).map(_ + "=").mkString("\n")
    Props.printableFile.foreach {
      fileName =>
        val f: File = new File(fileName)
        val file: BufferedSource = Source.fromFile(f)
        var keyMap = keyList.map(_ -> "").toMap

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

  def printableFile = Try(props.getString("i18n.print.directory")).toOption

  def isPrintable = Try(props.getBoolean("i18n.print.on")).getOrElse(false)
}
