package com.soulever.makro.types

/**
 * Created by tiran on 7/17/14.
 */
trait ImplicitTypes {

  implicit class Password(val get:String) {
    override def toString: String = "******"
  }

  implicit def password2String(p:Password):String = p.get

  implicit class Mapping[A](val get:A) {
    override def toString:String = Option(get).map(_.toString).getOrElse("")
  }

  implicit def mapping2a[A](value:Mapping[A]):A = value.get

  implicit class LongText(val value:String) {
    override def toString:String = value
  }

  implicit def longText2String(lt:LongText):String = lt.toString
}
