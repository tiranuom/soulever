package com.soulever.vaadin.test

import com.soulever.makro.annotations._
import com.soulever.makro.Soulever._
import com.soulever.vaadin.Forms._
import com.soulever.vaadin.{FieldDescriptorImplicits}
import com.vaadin.navigator.{View, ViewProvider}
import com.vaadin.ui._
import com.vaadin.navigator.ViewChangeListener.ViewChangeEvent

import scala.util.Try

class PersonViewProvider(ui:UI) extends ViewProvider {
  def getViewName(viewAndParameters: String): String = viewAndParameters

  def printPerson(person:TestCaseClass) = {
    new Notification("Operation Successful", s"Following data is saved $person").show(ui.getPage)
    Right[Exception, TestCaseClass](person)
  }

  implicit val imp = new Imp

  def getView(viewName: String): View = {

    val layout: VerticalLayout with View {def enter(event: ViewChangeEvent): Unit} = new VerticalLayout() with View {
      def enter(event: ViewChangeEvent) = {}
    }

    @min(0)
    val i:Int = 3

    val intField = field(i, "test.key", this)

    val person: TestCaseClass = new TestCaseClass(stringField = "name")

    layout.addComponent(new HorizontalLayout(form(person, submit(printPerson), reset), intField, imp.button("Submit", () => println(Try(intField.validate())))))
    layout
  }
}

case class TestCaseClass(@hidden @css("enum") @fieldDependent[Bool.Bool, TestCaseClass]((b, c) => b == c.enumeration2Field, "not-equal") enumerationField:Bool.Bool = Bool.TRUE,
                         @hidden enumeration2Field:Bool.Bool = Bool.TRUE,
                         @hidden @mapping[Imp, V](_.intMapping) mappedIntField:Mapping[V] = V(1),
                         @nonEmpty[String] stringField:String,
                         @hidden @nonEmpty[String] newStringField:String = "",
                         @hidden @min(0) @max(60) intField:Int = 0,
                         @hidden @min(0) @max(60) newIntField:Int = 0,
                         @hidden booleanField:Boolean = false,
                         @hidden passwordField:Password = "",
                         @hidden @mapping[Imp, V](_.intMapping) @nonEmpty[List[Option[Mapping[V]]]] listField:List[Option[Mapping[V]]] = List(None),
                         @hidden @custom[Option[Int]](TestCaseClass.customValidation, "all.positive") optionField:Option[Int] = None)

object TestCaseClass {
  def customValidation(o:Option[Int]) = o.map(_ > 0).getOrElse(true)
}

case class V(i:Int)

object Bool extends Enumeration {
  type Bool = Value
  val TRUE = Value("true")
  val FALSE = Value("false")
}

trait ServiceRegistry {
  lazy val intMapping:List[(String, V)] = (1 to 9).toList.map(i => "value" + i.toString -> V(i))
}

class Imp extends FieldDescriptorImplicits with com.soulever.vaadin.FieldDescriptor with ServiceRegistry