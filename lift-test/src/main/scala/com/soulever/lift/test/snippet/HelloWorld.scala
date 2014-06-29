package com.soulever.lift.test.snippet

import com.soulever.lift.{FormUtil, FieldDescriptorImplicits, FieldDescriptor}
import net.liftweb.http.js.jquery.JqJE.{JqRemove, Jq, JqAppend, JqId}

import scala.xml.{Elem, NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import com.soulever.lift.test.lib._
import Helpers._
import net.liftweb.http.{LiftRules, SHtml}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE._

class HelloWorld {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  // replace the contents of the element with id "time" with the date

//  val nameField = SHtml.ajaxText("test", {
//    a => println("from field : " + a)
//  })
//
//  val checkbox = SHtml.ajaxCheckbox(true, { b =>
//    val name = nameField.attributes("name")
//    Run(s"""document.getElementsByName("$name")[0].disabled=${if (!b) "\"disabled\"" else "null" }""")
//  })
//
//  def newField = {
//    val fieldId = LiftRules.funcNameGenerator()
//    val remove = SHtml.ajaxButton("-", () => JqId(fieldId).~>(JqRemove()).cmd)
//    val field = SHtml.ajaxText("test", {
//      s => println(s)
//    })
//    <li id={fieldId}>{field}{remove}</li>
//  }
//
//  val listId = LiftRules.funcNameGenerator.apply()
//
//  val addButton:Elem = SHtml.ajaxButton("+", () => JqId(listId).~>(JqAppend(newField)).cmd)
//
//  val submit = SHtml.submit("submit", () => {
//    println("no values here")
//  })

  implicit val imp = Imp

  val s = "hello"

  def howdy = {
    val field = FormUtil.field(s, "test", this)
    "#time *" #> date.map(_.toString) &
      "#form" #> field.elem
  }

}

object Imp extends FieldDescriptor with FieldDescriptorImplicits