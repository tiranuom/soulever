package com.soulever.makro
package annotations

import scala.annotation.StaticAnnotation

/**
 * @Auther tiran 
 * @Date 7/6/14.
 */
trait Attribute[A] {
  def key:String
  def value:A
}

case class cssClass(cls:String) extends StaticAnnotation with Attribute[String] {
  override def key: String = "class"

  override def value: String = cls
}