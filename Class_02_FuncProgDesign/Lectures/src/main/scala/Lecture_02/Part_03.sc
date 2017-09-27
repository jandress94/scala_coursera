// lazy evaluation

def expr = {
  val x = { print("x"); 1 }
  lazy val y = { print("y"); 2 }
  def z = { print("z"); 3 }
  z + y + x + z + y + x
}
val _result = expr

abstract class MyStream[+A] /*extends Seq[A]*/ {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]
  def filter(p: A => Boolean): MyStream[A] =
    if (isEmpty) this
    else if (p(head)) MyStream.cons(head, tail.filter(p))
    else tail.filter(p)
  def apply(index: Int): A =
    if (index == 0) head
    else tail.apply(index - 1)
}

object MyStream {
  def cons[T](hd: T, tl: => MyStream[T]) = new MyStream[T] {
    override def isEmpty = false
    override def head = hd
    lazy val tail = { print(hd + " "); tl }
//    lazy val tail = tl
//    override def tail = { print(hd + " "); tl }
  }

  val empty = new MyStream[Nothing] {
    override def isEmpty = true
    override def head = throw new NoSuchElementException("empty.head")
    override def tail = throw new NoSuchElementException("empty.tail")
  }
}

def streamRange(lo: Int, hi: Int): MyStream[Int] =
  if (lo >= hi) MyStream.empty
  else MyStream.cons(lo, streamRange(lo + 1, hi))

def isPrime(x: Int):Boolean = (2 until x) forall (i => x % i != 0)

val rng = streamRange(1000, 10000) filter isPrime

rng apply 1
rng apply 2
