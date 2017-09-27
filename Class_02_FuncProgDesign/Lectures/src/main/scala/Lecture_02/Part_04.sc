// Infinite Streams

def from(n: Int): Stream[Int] = n #:: from(n + 1)
val nats = from(0)
val multsOf4 = nats map (_ * 4)
(multsOf4 take 100).toList


// a stream of primes computed using the sieve of eristothenese
def primeStream: Stream[Int] = {
  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

  sieve(from(2))
}

(primeStream take 100).toList

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(2).take(6).toList

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess * guess - x) / x) < 0.0001

sqrtStream(2).filter(isGoodEnough(_, 2)).take(1).toList