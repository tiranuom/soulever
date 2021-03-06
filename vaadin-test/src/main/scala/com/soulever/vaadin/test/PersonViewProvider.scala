package com.soulever.vaadin.test

import com.soulever.makro.types.{Mapping, Password}
import com.soulever.vaadin.{FieldDescriptorImplicits, FormUtil}
import com.vaadin.navigator.{View, ViewProvider}
import com.vaadin.ui._
import com.vaadin.navigator.ViewChangeListener.ViewChangeEvent
import com.soulever.makro._
import com.soulever._

class PersonViewProvider(ui:UI) extends ViewProvider{
  def getViewName(viewAndParameters: String): String = viewAndParameters

  def printPerson(person:TestCaseClass) = {
    new Notification("Operation Successful", s"Following data is saved $person").show(ui.getPage)
    Right[Exception, TestCaseClass](person)
  }

  def getView(viewName: String): View = {

    val layout: VerticalLayout with View {def enter(event: ViewChangeEvent): Unit} = new VerticalLayout() with View {
      def enter(event: ViewChangeEvent) = {}
    }

    implicit val imp = new Imp

    val person: TestCaseClass = new TestCaseClass()

    layout.addComponent(FormUtil.form(person, printPerson))
    layout
  }
}

case class TestCaseClass( @field() enumeration:Bool.Bool = Bool.TRUE,
                          @field() @mapping[TestCaseClass, V](_.intMapping) mappedInt:Mapping[V] = V(0),
                          @field() @nonEmpty() stringField:String = "name",
                          @field() @min[Int](0) @max[Int](60) intField:Int = 0,
                          @field() booleanField:Boolean = false,
                          @field() passwordField:Password = "",
                          @field() listField:List[Option[Int]] = List(Some(4), Some(8), None),
                          @field() @custom[Option[Int]](_.map(_ > 0).getOrElse(true), message = "op") optionField:Option[Int] = None){
  def intMapping:List[(String, V)] = (1 to 9).toList.map(i => "value" + i.toString -> V(i))
}

case class V(i:Int)

object Bool extends Enumeration {
  type Bool = Value
  val TRUE = Value("true")
  val FALSE = Value("false")
}

class Imp extends FieldDescriptorImplicits with vaadin.FieldDescriptor
