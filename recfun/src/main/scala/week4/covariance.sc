//the problematic array ex from Java written in Scala
//Liskov substitution principle
import week3.*

val a: Array[NonEmpty] = Array(NonEmpty(1, Empty, Empty))
val b: Array[IntSet] = a
b(0) = Empty
val s: NonEmpty = a(0)

//Nil had to be a class, but we prefer to be an obj (there is only one empty list)
//can we change that? YES, by making List covariant
trait List[+T]
//Nothing <: T => List[Nothing] can be used for any List type
//Empty will be a subtype of any list the user might use
object Empty extends List[Nothing]

//Definition of lists that implements all the cases we seen so far
trait List[+T]:
  def isEmpty = this match
    case Nil => true
    case _ => false

  override def toString =
    def recur(prefix: String, xs: List[T]): String = xs match {
      case x :: xs1 => s"$prefix$x${recur(", ", xs1)}"
      case Nil => ")"
    }
    recur("List(", this)

//  this violates LSP because elem: T is covariant with +T (compiler should have failed)
  def prepend(elem: T): List[T] = ::(elem, this)

//making prepend covariant with a lower boiund
  def prependCo[U >: T](elem: U): List[U] = ::(elem, this)

  case class ::[+T](head: T, tail: List[T]) extends List[T]
  case object Nil extends List[Nothing]

//extension method - appears it does not work...
extension [T](x: T):
  def ::(xs: List[T]): List[T] = ::(x, xs)

object List:
  def apply() = Nil
  def apply[T](x: T) = x :: Nil
  def apply[T](x1: T, x2: T) = x1 :: x2 :: Nil
//  etc. we'll see a better method using vararg parameter