// subtyping in functions
// if A2 <: A1 and B1 <: B2
// then A1 => B1 <: A2 => B2

// functions are contravariant in their argument types and convariant in their result type
/*
trait Function1[-T, +U] {
  def apply(x: T): U
}
 */

// problem with covariance arises when you have a covariant type parameter T
// and a parameter of type T
/*
 * Roughly:
 * covariant type params can only appear in method results (may appear in lower bounds of method type params)
 * contravariant type params can only appear in method params (may appear in upper bounds of method type params)
 * invariant type params can appear anywhere
 */

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

val x: List[String] = Nil