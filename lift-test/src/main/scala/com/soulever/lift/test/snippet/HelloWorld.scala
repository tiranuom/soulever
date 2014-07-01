package com.soulever.lift.test.snippet

import com.soulever.lift.{FormUtil, FieldDescriptorImplicits, FieldDescriptor}
import com.soulever.makro.types.{Password, Mapping}
import com.soulever.makro._
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import com.soulever.lift.test.lib._
import Helpers._

import scala.xml.{NodeSeq, Elem}

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



  implicit val imp = new Imp

  val testCaseClass = TestCaseClass(stringField = "name")

  def howdy = {
    "#time *" #> date.map(_.toString) &
      "#form" #> FormUtil.form(testCaseClass, { (t: TestCaseClass) =>
        println(t)
        Right(t)
      })
  }

}

case class TestCaseClass(@field @css("enum") @fieldDependent[Bool.Bool, TestCaseClass]((b, c) => b == c.enumeration2Field, "not-equal") enumerationField:Bool.Bool = Bool.TRUE,
                         @field enumeration2Field:Bool.Bool = Bool.TRUE,
                         @field @mapping[Imp, V](_.intMapping) mappedIntField:Mapping[V] = V(1),
                         @field @nonEmpty stringField:String,
                         @field @min(0) @max(60) intField:Int = 0,
                         @field @min(0) @max(60) newIntField:Int = 0,
                         @field booleanField:Boolean = false,
                         @field passwordField:Password = "",
                         @field @mapping[Imp, V](_.intMapping) listField:List[Option[Mapping[V]]] = List(None),
                         @field @custom[Option[Int]]({(_:Option[Int]).map(_ > 0).getOrElse(true)}, "all.positive") optionField:Option[Int] = None)

case class V(i:Int)

object Bool extends Enumeration {
  type Bool = Value
  val TRUE = Value("true")
  val FALSE = Value("false")
}

trait ServiceRegistry {
  lazy val intMapping:List[(String, V)] = (1 to 9).toList.map(i => "value" + i.toString -> V(i))
}

class Imp extends FieldDescriptorImplicits with FieldDescriptor with ServiceRegistry