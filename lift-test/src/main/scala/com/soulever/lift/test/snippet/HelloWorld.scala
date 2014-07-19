package com.soulever.lift.test.snippet

import java.util.Date

import com.soulever.lift.test.lib._
import com.soulever.lift.{FieldDescriptor, FieldDescriptorImplicits}
import com.soulever.makro.Soulever._
import com.soulever.lift.Forms._
import com.soulever.makro.annotations._
import com.soulever.makro.providers.TypeEmptyProvider
import net.liftweb.common._
import net.liftweb.util.Helpers._

class HelloWorld {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  implicit val imp = new Imp

  type =/>[-A, +B] = PartialFunction[A, B]

  val f: Int =/> Int = {
    case i:Int => i
  }

  val testCaseClass = TestCaseClass(stringField = "name")

  val i = Some(2)

  implicit val stringTypeEmptyProvider = new TypeEmptyProvider[String] {
    override def empty: String = ""
  }

  def howdy = {
    "#time *" #> date.map(_.toString) &
      "#form" #> form(testCaseClass, submit[TestCaseClass, Unit](println), reset[TestCaseClass])

  }

  /*
  * FormUtil.form(testCaseClass, { (t: TestCaseClass) =>
        println(t)
        Right(t)
      })*/

  val F = form_*(testCaseClass, submit[TestCaseClass, Unit](println), reset[TestCaseClass])

}

case class TestCaseClass(@field @css("enum") @fieldDependent[Bool.Bool, TestCaseClass]((b, c) => b == c.enumeration2Field, "not-equal") enumerationField:Bool.Bool = Bool.TRUE,
                         @field enumeration2Field:Bool.Bool = Bool.TRUE,
                         @field @mapping[Imp, V](_.intMapping) mappedIntField:Mapping[V] = V(1),
                         @field @nonEmpty[String] stringField:String,
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