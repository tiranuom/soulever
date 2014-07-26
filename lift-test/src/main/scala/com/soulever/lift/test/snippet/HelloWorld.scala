package com.soulever.lift.test.snippet

import java.util.Date

import com.soulever.lift.Forms._
import com.soulever.lift.test.lib._
import com.soulever.lift.{Descriptor, FieldDescriptor, FieldDescriptorImplicits}
import com.soulever.makro.Soulever._
import com.soulever.makro.annotations._
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

  def howdy = {
    "#time *" #> date.map(_.toString) &
      "#form" #> form(testCaseClass, submit[TestCaseClass, Unit](println), reset[TestCaseClass])

  }

}

case class TestCaseClass(@hidden @css("enum") @fieldDependent[Bool.Bool, TestCaseClass]((b, c) => b == c.enumeration2Field, "not-equal") enumerationField:Bool.Bool = Bool.TRUE,
                         @hidden enumeration2Field:Bool.Bool = Bool.TRUE,
                         @mapping[Imp, V](_.intMapping) mappedIntField:Option[Mapping[V]] = Some(V(1)),
                         @hidden @hidden @nonEmpty[String] stringField:String,
                         @hidden @min(0) @max(60) intField:Int = 0,
                         @hidden @min(0) @max(60) newIntField:Int = 0,
                         @hidden booleanField:Boolean = false,
                         @hidden passwordField:Password = "",
                         @mapping[Imp, V](_.intMapping) listField:List[Option[Mapping[V]]] = List(None),
                         @hidden listField2:List[Option[Bool.Bool]] = List(None),
                         @hidden @custom[Option[Int]]({(_:Option[Int]).map(_ > 0).getOrElse(true)}, "all.positive") optionField:Option[Int] = None)

case class V(i:Int)

object Bool extends Enumeration {
  type Bool = Value
  val TRUE = Value("true")
  val FALSE = Value("false")
}

trait ServiceRegistry {
  lazy val intMapping:List[(String, V)] = (1 to 9).toList.map(i => "value" + i.toString -> V(i))
}

class Imp extends Descriptor with ServiceRegistry