package com.soulever.makro

package object types {
  implicit class Password(val get:String) extends AnyVal {
    override def toString: String = "******"
  }

  implicit def password2String(p:Password):String = p.get
}
