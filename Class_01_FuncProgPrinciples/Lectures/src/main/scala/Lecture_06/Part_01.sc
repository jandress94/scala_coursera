/*
 * Scala Collection Hierarchy
 *
 * Iterable
 *    - Seq
 *        - List
 *        - Vector
 *        - Range
 *        - String*
 *        - Array*
 *    - Set
 *    - Map
 *
 * * String and Array aren't actually Sequences because they come from Java, but are similar
 */

val xs = Array(1, 2, 3, 44)
xs.map(x => x * 2)

val str = "Hello World"
str.filter(c => c.isUpper)

val r: Range = 1 until 5
val s: Range = 1 to 5
1 to 10 by 3
6 to 1 by -2


str.exists(c => c.isUpper)
str.forall(c => c.isUpper)

val pairs = List(1, 2, 3) zip str
pairs.unzip

str flatMap (c => List('.', c))

xs.sum
xs.max

val M = 4
val N = 3

(1 to M) flatMap(x => (1 to N) map (y => (x, y)))

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2).sum

def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{case (x, y) => x * y}.sum

def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)

isPrime(2)
isPrime(4)
isPrime(97)
isPrime(99)