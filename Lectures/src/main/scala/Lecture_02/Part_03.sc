
def abs(x: Double) = if (x < 0) -x else x

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    println(guess)
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

fixedPoint(x => 1 + x / 2)(1)

def sqrt(x: Double) = fixedPoint(y => x / y)(1.0)

def sqrt_betterConvg(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)
// doesn't converge
//sqrt(2)
sqrt_betterConvg(2)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt_avgDamp(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)
sqrt_avgDamp(2.0)