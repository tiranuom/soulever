package com.soulever.vaadin.test

import com.soulever.makro.types.{Mapping, Password}
import com.soulever.vaadin.{FieldDescriptor, GeneratedField, FieldDescriptorImplicits, FormUtil}
import com.vaadin.navigator.{View, ViewProvider}
import com.vaadin.ui._
import com.vaadin.navigator.ViewChangeListener.ViewChangeEvent
import com.soulever.makro._
import com.soulever._
import com.typesafe.config.ConfigFactory
import scala.util.Try
import java.util.Properties

class PersonViewProvider(ui:UI) extends ViewProvider{
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

    import types._

    @min(0)
    val i:Int = 3

    val intField = FormUtil.field(i, "test.key", this)

    val person: TestCaseClass = new TestCaseClass(stringField = "name")

    layout.addComponent(new HorizontalLayout(FormUtil.form(person, printPerson), intField, imp.button("Submit", () => println(Try(intField.validate())))))
    I18nKeyCollector.print
    layout
  }
}

case class TestCaseClass(
                          @field @css("enum") @fieldDependent[Bool.Bool, TestCaseClass]((b, c) => b == c.enumeration2Field, "not-equal") enumerationField:Bool.Bool = Bool.TRUE,
                          @field enumeration2Field:Bool.Bool = Bool.TRUE,
                          @field @mapping[Imp, V](_.intMapping) mappedIntField:Mapping[V] = V(1),
                          @field @nonEmpty stringField:String,
                          @field @min(0) @max(60) intField:Int = 0,
                          @field @min(0) @max(60) newIntField:Int = 0,
                          @field booleanField:Boolean = false,
                          @field passwordField:Password = "",
                          @field @mapping[Imp, V](_.intMapping) listField:List[Option[Mapping[V]]] = List(None),
                          @field @custom[Option[Int]]({(_:Option[Int]).map(_ > 0).getOrElse(true)}, "all.positive") optionField:Option[Int] = None
                          ){
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

class Imp extends FieldDescriptorImplicits with vaadin.FieldDescriptor with ServiceRegistry