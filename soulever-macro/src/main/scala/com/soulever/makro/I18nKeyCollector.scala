package com.soulever.makro

import java.util

import com.typesafe.config.{ConfigValue, Config, ConfigFactory}
import scala.util.Try
import scala.io.{BufferedSource, Source}
import java.io.{File, PrintWriter}
import java.util.Properties

class I18nKeyCollector(printableFile:Option[String]) {

  val fileProps = new Properties()

  printableFile.foreach(name => Try(fileProps.load(Source.fromFile(new File(".").getAbsoluteFile + name).bufferedReader())) )

  var properties = Map.empty[String, String]

  def insert(key:String, defaultValue:String) {
    properties = properties + (key -> defaultValue)
  }

  def print = {
    printableFile.map { fileName =>
      import scala.collection.JavaConversions._
      fileProps.propertyNames().toList.foreach{
        name => properties = properties + (name.toString -> fileProps.getProperty(name.toString))
      }

      val writer: PrintWriter = new PrintWriter(new File(fileName))
      properties.foreach{
        case (key, defaultValue) =>
          println(s"$key=$defaultValue")
          writer.write(s"$key=$defaultValue\n")
      }
      writer.close()
    }
  }

  def i18n(s:String) = Try(fileProps.getProperty(s)).toOption.flatMap(Option.apply).getOrElse(properties.withDefaultValue(s)(s))
}
