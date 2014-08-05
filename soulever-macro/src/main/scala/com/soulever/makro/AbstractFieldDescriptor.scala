package com.soulever.makro

import com.soulever.makro.i18n.I18nKeyCollector
import com.soulever.makro.providers.{GeneratedFieldProvider, EmptyProvider, FieldProvider}
import Soulever._

trait AbstractFieldDescriptor[FieldDescriptor <: AbstractFieldDescriptor[FieldDescriptor]] {
  type LayoutType
  type FieldType[_]
  type ButtonType
  type BaseFieldType[A, Obj] <: BaseField[A, Obj]
  type RequestType

  def fieldComponent[A : Manifest, Obj](init:A,
                                        caption:String,
                                        innerField:(A, this.type#BaseFieldType[A, Obj]) => this.type#FieldType[A],
                                        validators:List[A => Either[String, A]] = List.empty,
                                        secondaryValidators:List[(A, Obj) => Either[String, A]] = List.empty,
                                        css:String = ""):this.type#BaseFieldType[A, Obj]

  def button(label:String, action:() => Any, fields:List[this.type#BaseFieldType[_, _]]):this.type#ButtonType

  def i18n(msg:String, defaultValue:Option[String] = None):String = i18nKeyCollector.i18n(msg, defaultValue)

  def formComponent(fields:List[this.type#BaseFieldType[_, _]],
                    buttons:List[this.type#ButtonType]):this.type#LayoutType

  def mappingEmptyProvider[A](mapping:List[(String, A)]) = new EmptyProvider[Mapping[A]] {
    override def empty: Soulever.Mapping[A] = mapping.headOption.map(a => Mapping(a._2)).orNull
  }

  def mappingFieldProvider[A](mapping:List[(String, A)]):GeneratedFieldProvider[Mapping[A], FieldDescriptor]

  def enumFieldProvider[A <: Enumeration](enum:A):FieldProvider[A#Value, FieldDescriptor]

  val i18nKeyCollector:I18nKeyCollector
}

trait BaseField[A, Obj] {

  def valid_? :Boolean

  def valid_?(obj:Obj):Boolean

  def value_=(value:A)

  def value:A

  def innerValidations:List[(String, String)]

  def innerI18nKeys:List[(String,String)]
}
