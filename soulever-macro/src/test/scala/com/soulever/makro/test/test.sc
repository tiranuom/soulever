trait A {
  type SelfType <: A
  type NextType
  def a:this.type#NextType
}

trait SubA extends A {
  type SelfType = SubA
  type NextType = Long
  override def a = 5
}

class Impl extends SubA

val a:SubA#NextType = (new Impl).a