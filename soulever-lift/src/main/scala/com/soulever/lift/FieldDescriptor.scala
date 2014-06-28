package com.soulever.lift

import com.soulever.lift.types.{TypeFieldProvider, InnerField, GeneratedField}
import com.soulever.makro.types.Mapping
import com.soulever.makro.MFieldDescriptor
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd

import scala.xml.{NodeSeq, Elem}

/**
 * @Auther tiran 
 * @Date 6/28/14.
 */
trait FieldDescriptor extends MFieldDescriptor[NodeSeq]{
  override type ButtonType = NodeSeq

  override type FieldType[A] = InnerField[A]

  override type BaseFieldType[A, Obj] = GeneratedField[A, Obj]

  override def field[A: Manifest, Obj](init: A,
                                       caption: String,
                                       innerField: (Option[A]) => FieldType[A],
                                       validators: List[(A) => Either[String, A]],
                                       secondaryValidators: List[(A, Obj) => Either[String, A]],
                                       css: String): BaseFieldType[A, Obj] =
    new GeneratedField[A, Obj](init, caption, innerField, validators, secondaryValidators, css)

  override def button(label: String, clickAction: () => Unit): ButtonType = ???

  override def form(fields: List[FieldType[_]], buttons: List[ButtonType]): Elem = ???

  override def mappingFieldProvider[A](mapping: List[(String, A)]): TypeFieldProvider[Mapping[A]] = ???

  override def enumFieldProvider[A <: Enumeration](enum: A): TypeFieldProvider[A#Value] = ???
}

trait FieldDescriptorImplicits {
  implicit val stringFieldProvider = new TypeFieldProvider[String] {
    override def field[FD <: MFieldDescriptor[_]](fieldDescriptor: FD, i18nKey: String)
                                                 (op: Option[String]): InnerField[String] = new InnerField[String] {
      
      var curValue = op.getOrElse("")
      private val text: Elem = SHtml.ajaxText(curValue, {s =>
        curValue = s
      })

      override def getValue: String = curValue

      override def setValue(value: String) = curValue = value

      override def elem: NodeSeq = text
    }

    override def empty: String = ""
  } 
}