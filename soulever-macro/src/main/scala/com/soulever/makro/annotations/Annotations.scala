package com.soulever.makro
package annotations

import scala.annotation.StaticAnnotation

case class field(value:String = "") extends StaticAnnotation

case class mapping[FD, A](value:(FD) => List[(String, A)]) extends StaticAnnotation

case class css(cls:String)