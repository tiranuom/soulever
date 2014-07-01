package com.soulever.lift.helpers

import net.liftweb.http.js.JsCmd

/**
 * @Auther tiran 
 * @Date 6/30/14.
 */
object JsCmdHelpers {
  case class ReplaceClass(id:String, oldClass: String, newClass: String) extends JsCmd {
    def toJsCmd =
      s"""try {
         var elem = document.getElementById("$id")
         elem.className = elem.className.replace("$oldClass", "") + "$newClass"
         } catch (e) {}
      """
//      """try {
//      $(""" + ("." + oldClass).encJs + """).each(function(){
//          $(this).addClass(""" + newClass.encJs + """).removeClass(""" + oldClass.encJs + """);
//        });
//    } catch (e) {}"""
  }
}
