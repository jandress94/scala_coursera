class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def neg: Rational = new Rational(-numer, denom)

  def add(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def sub(that: Rational) = add(that.neg)

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