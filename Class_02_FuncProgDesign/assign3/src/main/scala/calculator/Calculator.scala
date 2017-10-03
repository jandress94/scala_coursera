package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[_ <: Expr]]): Map[String, Signal[Double]] = {
    namedExpressions mapValues (expr => Signal(eval(expr(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[_ <: Expr]]): Double = {
    var namesUsed: Set[String] = Set()
    def eval_recurs(expr: Expr): Double = expr match {
      case Literal(v) => v
      case Ref(name) =>
        if (namesUsed.contains(name)) Double.NaN
        else {
          namesUsed += name
          eval_recurs(getReferenceExpr(name, references))
        }
      case Plus(a, b) => eval_recurs(a) + eval_recurs(b)
      case Minus(a, b) => eval_recurs(a) - eval_recurs(b)
      case Times(a, b) => eval_recurs(a) * eval_recurs(b)
      case Divide(a, b) => eval_recurs(a) / eval_recurs(b)
    }

    eval_recurs(expr)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[_ <: Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
