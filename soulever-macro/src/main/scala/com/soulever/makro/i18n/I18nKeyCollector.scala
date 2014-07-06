package com.soulever.makro
package i18n

import java.io.PrintWriter
import java.util.Date

import scala.annotation.tailrec
import scala.io.Source

/**
 * @Auther tiran 
 * @Date 7/6/14.
 */
class I18nKeyCollector(fileName:Option[String]) {

  private[i18n] var blocks = fileName.fold(List.empty[Block]) { fileName =>
    Source.fromFile(fileName).getLines().foldLeft(List.empty[Block]) {
      case (l, s) if s.startsWith("#") => l ::: List(Block(s))
      case (Nil, s) => List(Block("", Set(Property(s))))
      case (l, s) => l.init ::: List(l.last.copy(properties = l.last.properties + Property(s)))
    }
  }.zipWithIndex.map {
    case (block, index) => block.copy(key = block.comment.split("(\\[key=|\\])").toList.tail.headOption.getOrElse(s"$index"))
  }.distinct

  def addBlock(key:String) = {
    if (!blocks.exists(_.key == key))
      blocks = blocks ::: List(Block(s"""#i18n keys for "${key.naturalNotation}" [key=$key]""", key = key))
  }

  def insert(key:String, value:String) = {

    val replacaIndex: Option[Int] = {
      val selectables: List[(String, Int)] = blocks.zipWithIndex.flatMap {
        case (b, index) if key.startsWith(b.key) => Some(b.key -> index)
        case _ => None
      }
      if (selectables.isEmpty) None
      else Some(selectables.maxBy(_._1.length)._2)
    }

    def update(id:Int) = blocks.updated(id, blocks(id).copy(properties = blocks(id).properties + Property(key, value)))

    blocks = replacaIndex.fold {
      blocks.zipWithIndex.collectFirst{ case (a, id) if a.key == "__misc__" => id }.fold {
        blocks:::List(Block("# miscellanies keys [key=__misc__]", Set(Property(key, value)), "__misc__"))
      }(update)
    }(update)
  }

  def i18n(key:String, defaultValue:Option[String]) = {
    val property: Option[Property] = blocks.
      flatMap(_.properties).
      find(_.key == key)
    if(property.isEmpty) insert(key, defaultValue.getOrElse(key.naturalNotation))
    property.fold(defaultValue.getOrElse(key.naturalNotation))(_.value)
  }

  def print() {
    fileName.foreach{ file =>
      val writer: PrintWriter = new PrintWriter(file)
      writer.write(blocks.map(_.toString).mkString("\n\n"))
      writer.close()
    }
  }

  private[i18n] case class Property(key:String, value:String) {
    override def toString = s"$key=$value"
  }

  private[i18n] object Property {
    def apply(line:String):Property = {
      val parts: Array[String] = line.split("=")
      Property(parts.head, parts.tail.headOption.getOrElse(""))
    }
  }
  private[i18n] case class Block(comment:String, properties:Set[Property] = Set.empty, key:String = "") {
    override def toString =
      if (properties.filterNot(_.key.isEmpty).isEmpty) comment + "\n"
      else (comment :: properties.filterNot(_.key.isEmpty).toList.sortBy(_.key).map(_.toString)).mkString("\n")
  }

}
