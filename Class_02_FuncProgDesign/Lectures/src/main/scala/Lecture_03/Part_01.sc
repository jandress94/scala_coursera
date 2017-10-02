// mutable state
var x: String = "abc"
var count = 111

x = "hi"
count = count + 1


class BankAccount {
  private var balance = 0

  def deposit(amount: Int): Unit = {
    if (amount > 0) balance += amount
  }

  def withdraw(amount: Int): Int =
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      balance
    } else throw new Error("insufficient funds")
}

val account = new BankAccount
account deposit 50
account withdraw 20
account withdraw 20
//account withdraw 20


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
    private var tlOpt: Option[MyStream[T]] = None
    override def tail: MyStream[T] = tlOpt match {
      case Some(x) => x
      case None => tlOpt = Some(tl); tail
    }
  }

  val empty = new MyStream[Nothing] {
    override def isEmpty = true
    override def head = throw new NoSuchElementException("empty.head")
    override def tail = throw new NoSuchElementException("empty.tail")
  }
}