import scala.util.control.NonFatal
// Monads

trait M[T] {
  def flatMap[U](f: T => M[U]): M[U]  // AKA bind
}
def unit[T](x: T): M[T]

/*
 * List is monad w/ unit(x) = List(x)
 * Set is monad w/ unit(x) = Set(x)
 * Option is monad w/ unit(x) = Some(x)
 * Generator is monad with unit(x) = single(x)
 */

/*
 * m map f == m flatMap (x => unit(f(x)))
 *         -- m flatMap (f andThen unit)
 */

/*
 * Monad Laws
 * 1) Associativity:  (m flatMap f) flatMap g == m flatMap (x => f(x) flatMap g)
 * 2) Left unit:      unit(x) flatMap f == f(x)
 * 3) Right unit:     m flatMap unit == m
 */


// Try is not a monad (left unit fails) but has the "bullet-proof" guarantee that
// "an expression composed from Try, map, flatMap will never throw a non-fatal exception"
abstract class Try[+T] {
  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(x) => try f(x) catch { case NonFatal(ex) => Failure(ex) }
    case fail: Failure => fail
  }

  def map[U](f: T => U): Try[U] = this match {
    case Success(x) => Try(f(x))
    case fail: Failure => fail
  }
}
case class Success[T](x: T) extends Try[T]
case class Failure(ex: Throwable) extends Try[Nothing]

object Try {
  def apply[T](expr: => T): Try[T] =
    try Success(expr)
    catch {
      case NonFatal(ex) => Failure(ex)
    }
}