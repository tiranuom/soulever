package com.soulever.lift.experimental

import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => ru}
import ru._
import net.liftweb.http.js.JsCmd
import scala.util.matching.Regex

/**
 * @Auther tiran 
 * @Date 6/26/14.
 */
object JsMacro {
  implicit class JsMacroHelper(val sc:StringContext) {
    def js(args:Any*):String = macro JsMacroImpl.js_impl
  }

  private[JsMacro] class JsMacroImpl(val c:Context) {
    import c.universe._

    def js_impl(args:c.Expr[Any]*) = {
      println("c.prefix.tree = " + c.prefix.tree)

      val jsString:String = c.prefix.tree match {
        case q"com.soulever.lift.js.JsMacro.JsMacroHelper(scala.StringContext.apply(..$strings))" =>
          val parts = strings.map(_.toString).map { case r""""(.+)${a: String}"""" /*"""*/ => a}
          parts.head + parts.tail.zipWithIndex.map { case (part, id) => s"#$id$part"}.mkString
        case _ => c.abort(c.enclosingPosition, "unauthorized macro call")
      }

      println(jsString)

      q"${JsFixer.fix(jsString)}"
    }
  }

}//; is missing