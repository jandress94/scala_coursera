// Streams: Like Lists, but tail only computed on demand

Stream.cons(1, Stream.cons(2, Stream.empty))
Stream(1, 2, 3)
(1 to 1000).toStream


abstract class MyStream[+A] /*extends Seq[A]*/ {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]
  def filter(p: A => Boolean): MyStream[A] =
    if (isEmpty) this
    else if (p(head)) MyStream.cons(head, tail.filter(p))
    else tail.filter(p)
}

object MyStream {
  def cons[T](hd: T, tl: => MyStream[T]) = new MyStream[T] {
    override def isEmpty = false
    override def head = hd
    override def tail = tl
  }

  val empty = new MyStream[Nothing] {
    override def isEmpty = true
    override def head = throw new NoSuchElementException("empty.head")
    override def tail = throw new NoSuchElementException("empty.tail")
  }
}

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  print(lo + " ")
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

streamRange(1, 10).take(3).toList