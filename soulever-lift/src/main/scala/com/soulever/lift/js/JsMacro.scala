package com.soulever.lift.js

import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import net.liftweb.http.js.JsCmd
import scala.util.matching.Regex

/**
 * @Auther tiran 
 * @Date 6/26/14.
 */
object JsMacro {
  def jsCmd(jsString:String):JsCmd = macro JsMacroImpl.jsCmd_impl
}

class JsMacroImpl(c:Context) {
  import c.universe._

  implicit class RegexContext(sc: StringContext) {
    def r = {
      new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }
  }

  implicit class StringOps(s:String){
    def splitTpl(delimeter:String) = {
      s.split(delimeter) match {
        case Array(a, b @ _*) => a -> b.mkString(delimeter)
        case _ => s -> ""
      }

    }
  }

  def jsCmd_impl(jsString:c.Expr[String]) = {
    val jsStr = jsString match {
      case Literal(Constant(s: String)) => s
      case _ => c.abort(c.enclosingPosition, "js string required")
    }

    def inline(jsStr:String):String = ???

    def jsIfOptions(cond:String, body:String, body2:Option[String]) =
      body2.
        map(b => s"""JsIf(${createJsCmd(cond)}, ${createJsCmd(body)}, ${createJsCmd(b)})""").
        getOrElse(s"""JsIf(${createJsCmd(cond)}, ${createJsCmd(body)})""")

    def jsWhileOptions(cond:String, body:String) = s"""JsWhile(${createJsCmd(cond)}, ${createJsCmd(body)})"""

    def jsWithOptions(cond:String, body:String) = s"""JsWith(${createJsCmd(cond)}, ${createJsCmd(body)})"""

    def jsForOptions(init:String, cond:String, inc:String, body:String) =
      s"""JsFor(${createJsCmd(init)}, ${createJsCmd(cond)}, ${createJsCmd(inc)}, ${createJsCmd(body)})"""

    def jsForInOptions(init:String, ref:String, body:String) =
      s"""JsFor(${createJsCmd(init)}, "$ref", ${createJsCmd(body)})"""

    def createJsCmd(str:String):String = {
      str.trim match {
        //dataTypes
        case r"true" => s"JsTrue"
        case r"false" => s"JsFalse"
        case r"null" => s"JsNull"
        case r"_id" => s"Id"
        case r"_parent" => s"Parent"
        case r"_style" => s"Style"
        case r"_value" => s"Value"
        case r"(\d+)$a(\.\d+)?$b" => s"Num($a${Option(b).getOrElse("")})"
        //actions
        case r"JSON\.stringify\((.*)$a\)" => s"Stringify(${createJsCmd(a)})"
        case r"\[(.*)${a:String}\]" => s"JsArray(${a.split(",").map(createJsCmd).mkString(",")})"
        case r"""valById\("(.*)$a"\)""" => s"""ValById("$a")"""
        case r"""checkedById\("(.*)$a"\)""" => s"""CheckedById("$a")"""
        case r"elemById\(([^\.]*)$a(.*)$b\)" => s"""ElemById($a${b.split("\\.").tail.map(a => s""", "${a.replace("\"", "")}" """ ).filterNot(_.trim.isEmpty).mkString})"""
        case r"""parentOf\("(.*)$a"\)""" => s"""ParentOf("$a")"""
        case r"""jsHideId\("(.*)$a"\)""" => s"""JsHideId("$a")"""
        case r"""jsShowId\("(.*)$a"\)""" => s"""JsShowId("$a")"""
        case r"""replace\("(.*)$a",(.*)$b\)""" => s"""Replace("$a", $b)"""
        case r"""setHtml\("(.*)$a",(.*)$b\)""" => s"""SetHtml("$a", $b)"""
        case r"""focusOnLoad\((.*)$a\)""" => s"""FocusOnLoad($a)"""
        case r"""setValueAndFocus\("(.*)$a",(.*)$b\)""" => s"""SetValueAndFocus("$a", $b)"""
        case r"""focus\("(.*)$a"\)""" => s"""Focus("$a")"""
        case r"""onLoad\((.*)$a\)""" => s"""OnLoad(${createJsCmd(a)})"""
        case r"""setValById\("(.*)$a", (.*)$b\)""" => s"""SetValById("$a", $b)"""
        case r"""setElemById\("(.*)$a", (.*)$b\)""" => s"""SetValById("$a", $b)"""
        case r"(.*)$a=(.*)$b" => s"SetExp(${createJsCmd(a)},${createJsCmd(b)})"
        case r"var (.*)$a=(.*)$b" => s"JsCrVar("${a.trim}",${createJsCmd(b)})"
        case r"""setTimeout(\s*)${_}\((\s*)${_}function(\s*)${_}\(\)(\s*)${_}\{(.*)$b\}(\s*)${_},(\s*)${_}([\d]*)$d(\s*)${_}\)""" => s"After($b, ${createJsCmd(a)}})"
        case r"""alert(\s*)${_}\((\s*)${_}"(.*)$msg"(\s*)${_}\)""" => s"""Alert("$msg")"""
        case r"""prompt(\s*)${_}\((\s*)${_}"(.*)$msg"(\s*)${_},(\s*)${_}"(.*)$df"(\s*)${_}\)""" => s"""Prompt("$msg", "$df")"""
        case r"""confirm(\s*)${_}\((\s*)${_}"(.*)$msg"(\s*)${_},(\s*)${_}(.*)$a(\s*)${_}\)""" => s"""Confirm("$msg", ${createJsCmd(a)})"""
        case r"""run(\s*)${_}\((\s*)${_}"(.*)$cmd"(\s*)${_}\)""" => s"Run("$cmd")"
        case r"""try(\s*)${_}\{(.*)$block\}(\s*)${_}catch(\s*)${_}\((.*)${_}\)(\s*)${_}\{(.*)${_}\}""" => s"""JsTry(${createJsCmd(block)}, false)"""
        case r"""redirect(\s*)${_}\((\s*)${_}"(.*)$where"(\s*)${_}\)""" => s"""RedirectTo("$where")"""
        //redirect and function is not implemented.
        case r"""reload(\s*)${_}()""" => s"""Reload"""
        //replace options is not implemented.
        case r"""if(\s*)${_}\((.*)$cond\)(\s*)${_}\{(.*)$body\}((\s*)${_}else(\s*)${_}\{(.*)$body2\})?$e""" => jsIfOptions(cond, body, Option(body2))
        case r"""if(\s*)${_}\((.*)$cond\)(.*)$body(\s*)${_}(else(\s*)${_}\{(.*)$body2\})?$e""" => jsIfOptions(cond, body, Option(body2))
        case r"""if(\s*)${_}\((.*)$cond\)(.*)$body(\s*)${_}(else(.*)$body2)?$e""" => jsIfOptions(cond, body, Option(body2))
        case r"""if(\s*)${_}\((.*)$cond\)(\s*)${_}\{(.*)$body\}((\s*)${_}else(.*)$body2)?$e""" => jsIfOptions(cond, body, Option(body2))
        case r"""while(\s*)${_}\((.*)$cond\)(\s*)${_}\{(.*)$body\}""" => jsWhileOptions(cond, body)
        case r"""while(\s*)${_}\((.*)$cond\)(.*)$body""" => jsWhileOptions(cond, body)
        case r"""with(\s*)${_}\((.*)$ref\)(\s*)${_}\{(.*)$body\}""" => jsWithOptions(ref, body)
        case r"""with(\s*)${_}\((.*)$ref\)(.*)$body""" => jsWithOptions(ref, body)
        case r"""for(\s*)${_}\((.*)$init;(.*)$cond;(.*)$inc\)(\s*)${_}\{(.*)$body\}""" => jsForOptions(init, cond, inc, body)
        case r"""for(\s*)${_}\((.*)$init;(.*)$cond;(.*)$inc\)(.*)$body""" => jsForOptions(init, cond, inc, body)
        case r"""for(\s*)${_}\((.*)$init in (.*)$ref\)(\s*)${_}\{(.*)$body\}""" => jsForInOptions(init, ref, body)
        case r"""break""" => s"JsBreak"
        case r"""continue""" => s"JsContinue"
        case r"""return (.*)$value""" => Option(value).map(v => s"JsReturn($value)").getOrElse(s"JsReturn()")
        case r""""(.*)$a"""" => s"""Str("$a")"""
        case r"eval\((.*)$a\)" => s"""JsRaw("$a")"""
        case r"value_(.*)$a" => s"JsVal($a)"
        case r"function(\s+)$a([a-zA-Z$_]([a-zA-Z0-9$_])*$b)$c(\s*)$d\((.*)$e\)(\s*)$s3{(.*)$f}" => s"""Function("$b", List($e), ${createJsCmd(f)})"""
        case r"function(\s*)$s1\((.*)$p\)(\s*)$s2\{(.*)$b\}" => if(p.isEmpty) s"AnonFunc($b)" else s"""AnonFunc("$p", $b)"""
        case r"\((.*)$f\)\((.*)$p\)" => if(p.isEmpty) s"${createJsCmd(f)}.applied" else s"${createJsCmd(f)}.applied(${p.split(",").map(createJsCmd).mkString(",")})"
        case r"\{(([^:,]*)$b:([^:,]*)$c(,([^:,]*)$d:([^:,]*)$e)*$f)?$a\}" => s"JsObj(${b.split(",").toList.map{ a => val (a1, a2) = a.splitTpl(":");  s""" "$a1" -> ${createJsCmd(a2)}""" }}:_*)"

        case r"([a-zA-Z_$$][a-zA-Z0-9_$$]*_func)$a\((.*)$b\)" => s"JsFunc($a${b.split(",").map(a => s", ${createJsCmd(a)}").mkString})"
        case r"([a-zA-Z_$$][a-zA-Z0-9_$$]*)$a\((.*)$b\)" => s"Call($a${b.split(",").map(a => s", ${createJsCmd(a)}").mkString})"
        //operators
        case r"(.*)$a<(.*)$b" => s"JsLt(${createJsCmd(a)}, ${createJsCmd(b)})"
        case r"(.*)$a>(.*)$b" => s"JsGt(${createJsCmd(a)}, ${createJsCmd(b)})"
        case r"(.*)$a==(.*)$b" => s"JsEq(${createJsCmd(a)}, ${createJsCmd(b)})"
        case r"(.*)$a!=(.*)$b" => s"JsNotEq(${createJsCmd(a)}, ${createJsCmd(b)})"
        case r"(.*)$a<=(.*)$b" => s"JsLtEq(${createJsCmd(a)}, ${createJsCmd(b)})"
        case r"(.*)$a>=(.*)$b" => s"JsGtEq(${createJsCmd(a)}, ${createJsCmd(b)})"
        case r"(.*)$a||(.*)$b" => s"JsOr(${createJsCmd(a)}, ${createJsCmd(b)})"
        case r"(.*)$a&&(.*)$b" => s"JsAnd(${createJsCmd(a)}, ${createJsCmd(b)})"
        case r"!(.*)$a" => s"JsNot(${createJsCmd(a)})"
        case r"(.*)$a\.appendToParent\((.*)$b\)" => s"${createJsCmd(a)}.appendToParent($b)"
        case r"(.*)$a\.(.*)$b" => s"${createJsCmd(a)}.~>(${createJsCmd(b)})"
        case r"(.*)$a===(.*)$b" => s"${createJsCmd(a)}.===(${createJsCmd(b)})"
        case r"(.*)$a\+(.*)$b" => s"${createJsCmd(a)}.+(${createJsCmd(b)})"
        case r"(.*)$a;" => s"${createJsCmd(a)}.cmd"
        case s => s"$s"
      }
    }

  }
}
//; is missing