class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

  def neg: Rational = new Rational(-numer, denom)

  def add(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def sub(that: Rational) = add(that.neg)

  def less(that: Rational): Boolean = numer * that.denom < that.numer * denom

  def max(that: Rational): Rational = if (this.less(that)) that else this

  override def toString = numer + "/"  + denom
}

val x = new Rational(1, 3)
x.numer
x.denom

val y = new Rational(5, 7)
x.add(y)

val z = new Rational(3, 2)

//x.add(y).mul(z)

x.sub(y).sub(z)

y.add(y)

x.max(y)

x.less(y)

//val illegal = new Rational(1, 0)
new Rational(4)