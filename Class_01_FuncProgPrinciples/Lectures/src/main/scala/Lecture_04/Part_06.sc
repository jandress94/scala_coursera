trait Expr {
  override def toString: String = this match {
    case Number(n) => n.toString
    case Sum(e1, e2) => "(" + e1 + ") + (" + e2 + ")"
  }
}
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

val num1 = Number(1)
val sum2_7 = Sum(Number(2), Number(7))

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

eval(num1)

eval(sum2_7)