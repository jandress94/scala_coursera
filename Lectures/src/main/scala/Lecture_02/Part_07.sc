class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

  def unary_- : Rational = new Rational(-numer, denom)

  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def -(that: Rational) = this + (-that)

  def <(that: Rational): Boolean = numer * that.denom < that.numer * denom

  def max(that: Rational): Rational = if (this < that) that else this

  override def toString = numer + "/"  + denom
}

val x = new Rational(1, 3)
x.numer
x.denom

val y = new Rational(5, 7)
x + y

val z = new Rational(3, 2)

//x.add(y).mul(z)

x - y - z

y + y

x max y

x < y

//val illegal = new Rational(1, 0)
new Rational(4)