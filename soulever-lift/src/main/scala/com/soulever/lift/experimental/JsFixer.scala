package com.soulever.lift.experimental



/**
 * @Auther tiran 
 * @Date 6/28/14.
 */
object JsFixer {
  val semicolonInvalidList = List("^$",
    "(.*)[;\\{\\}]$",
    "(if|for|while)(\\s*)(\\((.*)\\))(\\s*)$",
    "(.*)else$")

  def fix(js:String) = {
    js.replace("\\n", "\n").split("\n").toList.map{
      case s if semicolonInvalidList.exists(s.trim.matches) => s.trim + ""
      case s => s.trim + ";"
    }.mkString.replace(";", ";\n").split("\n").toList.map{
      case s @ r"(.*)${pre}if(\s*)${_}\((.*)$cond\)⤴(\s*)${_}([^{]$b(.*)$a)$body" =>
        println(s"pre = $pre : cond = $cond : b = $b, a = $a : body = $body")
        s
      case s => s
    }.mkString("\n")
  }
}
