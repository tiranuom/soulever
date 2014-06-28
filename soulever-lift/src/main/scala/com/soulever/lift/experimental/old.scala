package com.soulever.lift.experimental

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */
class old {
  //
  //    def inline(jsStr:String):String = ???
  //
  //    implicit class StringOps(s:String){
  //      def splitTpl(delimeter:String) = {
  //        s.split(delimeter) match {
  //          case Array(a, b @ _*) => a -> b.mkString(delimeter)
  //          case _ => s -> ""
  //        }
  //
  //      }
  //    }
  //
  //    def jsIfOptions(cond:String, body:String, body2:Option[String]) =
  //      body2.
  //        map(b => s"""JsIf(${createJsCmd(cond)}, ${createJsCmd(body)}, ${createJsCmd(b)})""").
  //        getOrElse(s"""JsIf(${createJsCmd(cond)}, ${createJsCmd(body)})""")
  //
  //    def jsWhileOptions(cond:String, body:String) = s"""JsWhile(${createJsCmd(cond)}, ${createJsCmd(body)})"""
  //
  //    def jsWithOptions(cond:String, body:String) = s"""JsWith(${createJsCmd(cond)}, ${createJsCmd(body)})"""
  //
  //    def jsForOptions(init:String, cond:String, inc:String, body:String) =
  //      s"""JsFor(${createJsCmd(init)}, ${createJsCmd(cond)}, ${createJsCmd(inc)}, ${createJsCmd(body)})"""
  //
  //    def jsForInOptions(init:String, ref:String, body:String) =
  //      s"""JsFor(${createJsCmd(init)}, ${createJsCmd(ref)}, ${createJsCmd(body)})"""
  //
  //    def jsDoWhile(body:String, cond:String) = s"""JsDoWhile(${createJsCmd(body)}, ${createJsCmd(cond)})"""
  //
  //    def createJsCmd(str:String):String = {
  //      str.trim match {
  //        //dataTypes
  //        case r"true" => s"JsTrue"
  //        case r"false" => s"JsFalse"
  //        case r"null" => s"JsNull"
  //        case r"_id" => s"Id"
  //        case r"_parent" => s"Parent"
  //        case r"_style" => s"Style"
  //        case r"_value" => s"Value"
  //        case r"""if(\s*)${_}\(([^\}]*)$cond\)(\s*)${_}\{([^}]*)$body\}(\s*)${_}else(\s*)${_}\{(.*)$body2\}""" => jsIfOptions(cond, body, Option(body2))
  //        case r"""if(\s*)${_}\(([^\}]*)$cond\)(\s*)${_}\{([^}]*)$body\}""" => jsIfOptions(cond, body, None)
  //        case r"""while(\s*)${_}\((.*)$cond\)(\s*)${_}\{(.*)$body\}""" => jsWhileOptions(cond, body)
  //        case r"""with(\s*)${_}\((.*)$ref\)(\s*)${_}\{(.*)$body\}""" => jsWithOptions(ref, body)
  //        case r"""do(\s*)${_}\{$body\}(\s*)${_}while(\s*)${_}\($cond\)""" => jsDoWhile(body, cond)
  //        case r"""for(\s*)${_}\((.*)$init;(.*)$cond;(.*)$inc\)(\s*)${_}\{(.*)$body\}""" => jsForOptions(init, cond, inc, body)
  //        case r"""for(\s*)${_}\((.*)$init in (.*)$ref\)(\s*)${_}\{(.*)$body\}""" => jsForInOptions(init, ref, body)
  //        case r"(\d+)$a(\.\d+)?$b" => s"Num($a${Option(b).getOrElse("")})"
  //        //actions
  //        case r"stringify(\s*)${_}\((.*)$a\)" => s"Stringify(${createJsCmd(a)})"
  //        case r"\[(.*)${a:String}\]" => s"JsArray(${a.split(",").map(createJsCmd).mkString(",")})"
  //        case r"""valById(\s*)${_}\((\s*)${_}"(.*)$a"(\s*)${_}\)""" => s"""ValById("$a")"""
  //        case r"""checkedById(\s*)${_}\((\s*)${_}"(.*)$a"(\s*)${_}\)""" => s"""CheckedById("$a")"""
  //        case r"elemById(\s*)${_}\(([^\.]*)$a(.*)$b\)" => s"""ElemById($a${b.split("\\.").tail.map(a => s""", "${a.replace("\"", "")}" """ ).filterNot(_.trim.isEmpty).mkString})"""
  //        case r"""parentOf(\s*)${_}\((\s*)${_}"(.*)$a"(\s*)${_}\)""" => s"""ParentOf("$a")"""
  //        case r"""jsHideId(\s*)${_}\((\s*)${_}"(.*)$a"(\s*)${_}\)""" => s"""JsHideId("$a")"""
  //        case r"""jsShowId(\s*)${_}\((\s*)${_}"(.*)$a"(\s*)${_}\)""" => s"""JsShowId("$a")"""
  //        case r"""replace(\s*)${_}\((\s*)${_}"(.*)$a"(\s*)${_},(.*)$b\)""" => s"""Replace("$a", $b)"""
  //        case r"""setHtml(\s*)${_}\("(\s*)${_}(.*)$a"(\s*)${_},(.*)$b\)""" => s"""SetHtml("$a", $b)"""
  //        case r"""focusOnLoad(\s*)${_}\((.*)$a\)""" => s"""FocusOnLoad($a)"""
  //        case r"""setValueAndFocus(\s*)${_}\((\s*)${_}"(.*)$a"(\s*)${_},(.*)$b\)""" => s"""SetValueAndFocus("$a", $b)"""
  //        case r"""focus(\s*)${_}\((\s*)${_}"(.*)$a"(\s*)${_}\)""" => s"""Focus("$a")"""
  //        case r"""onLoad(\s*)${_}\((.*)$a\)""" => s"""OnLoad(${createJsCmd(a)})"""
  //        case r"""setValById(\s*)${_}\((\s*)${_}"(.*)$a"(\s*)${_},(.*)$b\)""" => s"""SetValById("$a", $b)"""
  //        case r"""setElemById(\s*)${_}\((\s*)${_}"(.*)$a"(\s*)${_},(.*)$b\)""" => s"""SetValById("$a", $b)"""
  //        case r"""setTimeout(\s*)${_}\((\s*)${_}function(\s*)${_}\(\)(\s*)${_}\{(.*)$b\}(\s*)${_},(\s*)${_}([\d]*)$d(\s*)${_}\)""" => s"After(${createJsCmd(b)}, ${createJsCmd(d)})"
  //        case r"""alert(\s*)${_}\((\s*)${_}"(.*)$msg"(\s*)${_}\)""" => s"""Alert("$msg")"""
  //        case r"""prompt(\s*)${_}\((\s*)${_}"(.*)$msg"(\s*)${_},(\s*)${_}"(.*)$df"(\s*)${_}\)""" => s"""Prompt("$msg", "$df")"""
  //        case r"""confirm(\s*)${_}\((\s*)${_}"(.*)$msg"(\s*)${_},(\s*)${_}(.*)$a(\s*)${_}\)""" => s"""Confirm("$msg", ${createJsCmd(a)})"""
  //        case r"""run(\s*)${_}\((\s*)${_}"(.*)$cmd"(\s*)${_}\)""" => s"""Run("$cmd")"""
  //        case r"""try(\s*)${_}\{(.*)$block\}(\s*)${_}catch(\s*)${_}\((.*)${_}\)(\s*)${_}\{(.*)${_}\}""" => s"""JsTry(${createJsCmd(block)}, false)"""
  //        case r"""redirect(\s*)${_}\((\s*)${_}"(.*)$where"(\s*)${_}\)""" => s"""RedirectTo("$where")"""
  //        //redirect and function is not implemented.
  //        //replace options is not implemented.
  //        case r"""reload(\s*)${_}()""" => s"""Reload"""
  //        case r"""break""" => s"JsBreak"
  //        case r"""continue""" => s"JsContinue"
  //        case r"""return(.*)$value""" => Option(value).map(v => s"JsReturn(${createJsCmd(value)})").getOrElse(s"JsReturn()")
  //        case r""""(.*)$a"""" => s"""Str("$a")"""
  //        case r"eval\((.*)$a\)" => s"""JsRaw("$a")"""
  //        case r"value_(.*)$a" => s"JsVal($a)"
  //        case r"function(\s+)$a([a-zA-Z$$_]([a-zA-Z0-9$$_])*$b)$c(\s*)$d\((.*)$e\)(\s*)$s3\{(.*)$f\}" => s"""Function("$b", List($e), ${createJsCmd(f)})"""
  //        case r"function(\s*)$s1\((.*)$p\)(\s*)$s2\{(.*)$b\}" => if(p.isEmpty) s"AnonFunc(${createJsCmd(b)})" else s"""AnonFunc("$p", ${createJsCmd(b)})"""
  //        case r"\((.*)$f\)\((.*)$p\)" => if(p.isEmpty) s"${createJsCmd(f)}.applied" else s"${createJsCmd(f)}.applied(${p.split(",").map(createJsCmd).mkString(",")})"
  //        case r"\{(([^:,]*)$b:([^:,]*)$c(,([^:,]*)$d:([^:,]*)$e)*$f)?$a\}" => s"JsObj(${b.split(",").toList.map{ a => val (a1, a2) = a.splitTpl(":");  s""" "$a1" -> ${createJsCmd(a2)}""" }}:_*)"
  //        case r"var (\s*)${_}(.*)$a=(.*)$b" => s"""JsCrVar("${a.trim}",${createJsCmd(b)})"""
  //        case r"var (\s*)${_}(.*)$a" => s"""JsCrVar("${a.trim}",null)"""
  //        case r"([a-zA-Z_$$][a-zA-Z0-9_$$]*_func)$a\((.*)$b\)" => s"JsFunc($a${b.split(",").map(a => s", ${createJsCmd(a)}").mkString})"
  //        case r"([a-zA-Z_$$][a-zA-Z0-9_$$]*)$a\((.*)$b\)" => s"Call($a${b.split(",").map(a => s", ${createJsCmd(a)}").mkString})"
  //        //operators
  //        case r"(.*)$a\|\|(.*)$b" => s"JsOr(${createJsCmd(a)}, ${createJsCmd(b)})"
  //        case r"(.*)$a&&(.*)$b" => s"JsAnd(${createJsCmd(a)}, ${createJsCmd(b)})"
  //        case r"(.*)$a<(.*)$b" => s"JsLt(${createJsCmd(a)}, ${createJsCmd(b)})"
  //        case r"(.*)$a>(.*)$b" => s"JsGt(${createJsCmd(a)}, ${createJsCmd(b)})"
  //        case r"(.*)$a==(.*)$b" => s"JsEq(${createJsCmd(a)}, ${createJsCmd(b)})"
  //        case r"(.*)$a===(.*)$b" => s"${createJsCmd(a)}.===(${createJsCmd(b)})"
  //        case r"(.*)$a!=(.*)$b" => s"JsNotEq(${createJsCmd(a)}, ${createJsCmd(b)})"
  //        case r"(.*)$a<=(.*)$b" => s"JsLtEq(${createJsCmd(a)}, ${createJsCmd(b)})"
  //        case r"(.*)$a>=(.*)$b" => s"JsGtEq(${createJsCmd(a)}, ${createJsCmd(b)})"
  //        case r"!(.*)$a" => s"JsNot(${createJsCmd(a)})"
  //        case r"(.*)$a\.appendToParent\((.*)$b\)" => s"${createJsCmd(a)}.appendToParent($b)"
  //        case r"(.*)$a=(.*)$b" => s"SetExp(${createJsCmd(a)},${createJsCmd(b)})"
  //        case r"(.*)$a\.(.+)$b" => s"${createJsCmd(a)}.~>(${createJsCmd(b)})"
  //        case r"(.*)$a\*(.*)$b" => s"""JsRaw("${createJsCmd(a)} * ${createJsCmd(b)}")"""
  //        case r"(.*)$a/(.*)$b" => s"""JsRaw("${createJsCmd(a)} / ${createJsCmd(b)}")"""
  //        case r"(.*)$a\+(.*)$b" => s"${createJsCmd(a)}.+(${createJsCmd(b)})"
  //        case r"(.*)$a\+=(.*)$b" => s"""JsRaw("${createJsCmd(a)} += ${createJsCmd(b)}")"""
  //        case r"(.*)$a-(.*)$b" => s"""JsRaw("${createJsCmd(a)} -= ${createJsCmd(b)}")"""
  //        case r"(.*)$a-=(.*)$b" => s"""JsRaw("${createJsCmd(a)} - ${createJsCmd(b)}")"""
  //        case r"(.*)$a++" => s"""JsRaw("${createJsCmd(a)}++")"""
  //        case r"(.*)$a--" => s"""JsRaw("${createJsCmd(a)}--")"""
  //        case r"(.*)$a;(.*)$b" => s"${createJsCmd(a)}.&(${createJsCmd(b)})"
  //        case r"(.*)$a;" => s"${createJsCmd(a)}.cmd"
  //        case s =>
  //          println(s)
  //          s"$s"
  //      }
  //    }
}
