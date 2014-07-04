import scala.collection.generic.GenericCompanion

trait TypeEmptyProvider[A] { def empty : A}
trait KindEmptyProvider[+A[_]] { def empty[B, C[_] <: A[_]] : C[B]}

//implicit val listEmptyProvider = new KindEmptyProvider[List] {
//  override def empty[B, C[_] <: List[_]]: List[B] = List.empty[B]
//}
//
//implicit val optionEmptyProvider = new KindEmptyProvider[Option] {
//  override def empty[B]: Option[B] = None
//}
//
//class Wrapper[-A[_]] {
//  def field[B, C[_] <: A[_]](a:C[B]):List[C[B]] = List(a)
//  def empty[C[_] <: A[_], B](implicit ep:KindEmptyProvider[C]):C[B] = ep.empty[B]
//}
//new Wrapper[Seq].empty[List, Option[Int]]

