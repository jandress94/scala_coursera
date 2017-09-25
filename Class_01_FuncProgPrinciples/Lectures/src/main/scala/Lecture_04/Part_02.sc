trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  override def toString: String = {
    def peelLayer[T](l: List[T], accum: String): String = {
      if (l.isEmpty) accum + "]"
      else peelLayer(l.tail, accum + l.head + " ")
    }
    peelLayer(this, "[ ")
  }
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
  def apply[T](): List[T] = new Nil

  def apply[T](x: T): List[T] = new Cons(x, new Nil)

  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
}

List()

List(1)

List(2, 3)