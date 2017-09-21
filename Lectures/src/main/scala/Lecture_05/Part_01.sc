// returns a list which is everything but the last element of a list
def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ::: List(y)
}

def removeAt[T](xs: List[T], n: Int): List[T] =
  if (xs.isEmpty) xs
  else if (n == 0) xs.tail
  else xs.head :: removeAt(xs.tail, n - 1)

def removeAt2[T](xs: List[T], n: Int): List[T] = xs.take(n) ::: xs.drop(n + 1)

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => xs
  case y :: ys => y match {
    case l: List[Any] => flatten(l) ::: flatten(ys)
    case _ => y :: flatten(ys)
  }
}

val l = List(0, 1, 2, 3, 4, 5)
val l2 = List (6, 7, 8, 9, 0)

init(l)
init(l2)

concat(l, l2)
concat(l2, l)

reverse(l)
reverse(l2)

removeAt(l, 0)
removeAt2(l, 0)
removeAt(l, 1)
removeAt2(l, 1)
removeAt(l, 4)
removeAt2(l, 4)
removeAt(l, 10)
removeAt2(l, 10)

flatten(List(List(1, 1), 2, List(3, List(5, 8))))