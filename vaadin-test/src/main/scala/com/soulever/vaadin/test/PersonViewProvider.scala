package com.soulever.vaadin.test

import com.soulever.vaadin.{FieldDescriptorImplicits, FormUtil}
import com.vaadin.navigator.{View, ViewProvider}
import com.vaadin.ui._
import com.vaadin.navigator.ViewChangeListener.ViewChangeEvent
import com.soulever.makro._
import com.soulever._

class PersonViewProvider extends ViewProvider{
  def getViewName(viewAndParameters: String): String = viewAndParameters

  def printPerson(person:UniversityStudent) = {
    println(person)
    Right[Exception, UniversityStudent](person)
  }

  def getView(viewName: String): View = {

    val layout: VerticalLayout with View {def enter(event: ViewChangeEvent): Unit} = new VerticalLayout() with View {
      def enter(event: ViewChangeEvent) = {}
    }

    implicit val imp = new Imp

    val person: UniversityStudent = new UniversityStudent()

    layout.addComponent(FormUtil.form(person, printPerson))
    layout
  }
}

case class UniversityStudent(@field() @nonEmpty() name:String = "name",
                             @field() @min[Int](0) @max[Int](60) age:Int = 0,
                             @field() @regex("(\\d){11}") phoneNumber:String = "",
                             @field() @custom[Option[Int]](_.map(_ > 0).getOrElse(true), message = "op") optionalParam:Option[Int] = None)

class Imp extends FieldDescriptorImplicits with vaadin.FieldDescriptor
