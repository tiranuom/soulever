package com.soulever.lift.test

import com.soulever.lift.experimental.JsMacro

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */
class Test {
  import JsMacro._
  implicit class RegexContext(sc: StringContext) {
    object r {
      def apply (args : Any*) : String = {
        sc.s (args.zipWithIndex.map(a => "#" + a._2) : _*)
      }

      def unapplySeq (s : String) : Option[Seq[String]] = {
        val regexp = sc.parts.mkString ("(.+)").r
        regexp.unapplySeq (s)
      }
    }
  }
//  implicit class JsContext(sc: StringContext) {
//    def js(args: Any*) = {
//      JsMacro.jsCmd(sc.s(args.zipWithIndex.map(a => "#" + a._2) : _*))
//    }
//  }

//  val name = "identity"
//  js"function $name(a) {return a}"
}
