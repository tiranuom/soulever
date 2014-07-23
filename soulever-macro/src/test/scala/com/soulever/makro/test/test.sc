trait EmptyProvider[A] {
  def empty:A
}

class IntEmptyProvider extends EmptyProvider[Int] {
  override def empty: Int = 0
}

class OptionEmptyProvider[A : EmptyProvider] extends EmptyProvider[Option[A]] {
  override def empty: Option[A] = Some(implicitly[EmptyProvider[A]].empty)
}

class ListEmptyProvider[A : EmptyProvider] extends EmptyProvider[List[A]] {
  override def empty: List[A] = List(implicitly[EmptyProvider[A]].empty)
}

object test {
  implicit val stringEmptyProvider = new EmptyProvider[String] {
    override def empty: String = ""
  }

  implicit val intEmptyProvider = new IntEmptyProvider

  implicit def optionEmptyProvider[A : EmptyProvider] = new OptionEmptyProvider[A]

  implicit def listEmptyProvider[A : EmptyProvider] = new ListEmptyProvider[A]

  def empty[A:EmptyProvider] = implicitly[EmptyProvider[A]].empty

  empty[List[Option[String]]]
}