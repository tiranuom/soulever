trait Empty[A] {
  def empty:A
}
object test {
  trait Raw1 {
    implicit val intEmpty = new Empty[Int] {
      override def empty: Int = 0
    }
  }
  object Raw extends Raw1

  object Cons {
    implicit def optionEmpty[A : Empty] = new Empty[Option[A]] {
      override def empty: Option[A] = Some(implicitly[Empty[A]].empty)
    }
  }
  object test{
  }
  import Raw._
  import Cons._
  def empty[A:Empty] = implicitly[Empty[A]].empty
  println(empty[Option[Int]])
}
