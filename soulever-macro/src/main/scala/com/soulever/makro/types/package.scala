package com.soulever.makro

package object types {
  implicit class Password(val get:String) {
    override def toString: String = "******"
  }

  implicit def password2String(p:Password):String = p.get

  implicit class Mapping[A](val get:A) {
    override def toString:String = get.toString
  }

  implicit def mapping2a[A](value:Mapping[A]):A = value.get
//
//  implicit class Mapped[A](val get:(A, Map[String, A])) {
//    def value = get._1
//    def mapping = get._2
//    override def toString: String = value.toString
//  }
//
//  implicit def mapped2a[A](value:Mapped[A]):A = value.value

  implicit class LongText(val value:String) {
    override def toString:String = value
  }

  implicit def longText2String(lt:LongText):String = lt.toString
}
