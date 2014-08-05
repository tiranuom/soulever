trait GeneratedEmpty[A] {
  def empty : A
}

trait Empty[A] {
  def empty : A
}

trait LowPriorityProviders {
  implicit def emptyProvider[A:GeneratedEmpty] = new Empty[A] {
    override def empty: A = implicitly[GeneratedEmpty[A]].empty
  }
}

trait FrameworkProviders extends LowPriorityProviders {
//  implicit val definedIntEmpty = new Empty[Int] {
//    override def empty: Int = 1
//  }
}

trait OverriddenProviders extends FrameworkProviders {
//  implicit val oDefinedIntEmpty = new Empty[Int] {
//    override def empty: Int = 2
//  }
}

trait GeneratedProviders {
  implicit val intEmpty = new GeneratedEmpty[Int] {
    override def empty: Int = 0
  }
}

val implicits = new OverriddenProviders with GeneratedProviders {}

import implicits._
def empty[A:Empty] = implicitly[Empty[A]].empty
empty[Int]
