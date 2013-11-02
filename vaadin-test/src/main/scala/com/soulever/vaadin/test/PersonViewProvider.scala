package com.soulever.vaadin.test

import com.soulever.makro.types.Password
import com.soulever.vaadin.{FieldDescriptorImplicits, FormUtil}
import com.vaadin.navigator.{View, ViewProvider}
import com.vaadin.ui._
import com.vaadin.navigator.ViewChangeListener.ViewChangeEvent
import com.soulever.makro._
import com.soulever._

class PersonViewProvider extends ViewProvider{
  def getViewName(viewAndParameters: String): String = viewAndParameters

  def printPerson(person:TestCaseClass) = {
    println(person)
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

case class TestCaseClass(@field() @nonEmpty() stringField:String = "name",
                         @field() @min[Int](0) @max[Int](60) intField:Int = 0,
                         @field() booleanField:Boolean = false,
                         @field() passwordField:Password = "",
                         @field() listField:List[Option[String]] = List(Some("hello"), Some("world"), None),
                         @field() @custom[Option[Int]](_.map(_ > 0).getOrElse(true), message = "op") optionField:Option[Int] = None)

class Imp extends FieldDescriptorImplicits with vaadin.FieldDescriptor
