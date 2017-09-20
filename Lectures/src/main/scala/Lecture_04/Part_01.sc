abstract class MyBoolean {
  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => MyBoolean): MyBoolean = ifThenElse(x, False)
  def || (x: => MyBoolean): MyBoolean = ifThenElse(True, x)
  def unary_! : MyBoolean = ifThenElse(False, True)

  def == (x: MyBoolean): MyBoolean = ifThenElse(x, x.unary_!)
  def != (x: MyBoolean): MyBoolean = ifThenElse(x.unary_!, x)

  def < (x: MyBoolean): MyBoolean = ifThenElse(False, x)
}

object True extends MyBoolean {
  def ifThenElse[T](t: => T, e: => T) = t
}

object False extends MyBoolean {
  def ifThenElse[T](t: => T, e: => T) = e
}




// Peano Numbers
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {

  override def isZero: Boolean = true

  override def predecessor: Nat = throw new NoSuchElementException("Zero has no predecessor")

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) Zero else throw new ArithmeticException("Negative result")

  override def toString: String = "0"
}

class Succ(n: Nat) extends Nat {

  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = this.predecessor + that.successor

  override def -(that: Nat): Nat = if (that.isZero) this else this.predecessor - that.predecessor

  override def toString: String = {
    def peelLayer(x: Nat): Int = if (x.isZero) 0 else 1 + peelLayer(x.predecessor)
    peelLayer(this).toString
  }
}

val zero = Zero
val one = zero.successor
val two = one.successor
val three = two.successor
val four = three.successor

two + four
three - two

four - two
four - zero

zero + two
three + zero

four.successor
four.predecessor