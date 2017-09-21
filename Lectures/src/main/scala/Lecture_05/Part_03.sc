def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2

  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) => if (lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
    }

    val (fst, snd) = xs.splitAt(n)
    merge(msort(fst)(lt), msort(snd)(lt))
  }
}

val nums = List(2, -4, 7, 5, 1)
msort(nums)((x, y) => x < y)

val fruits = List("apple", "pineapple", "orange", "banana")
msort(fruits)((x, y) => x.compareTo(y) < 0)


import math.Ordering

def msort_ord[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2

  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
    }

    val (fst, snd) = xs.splitAt(n)
    merge(msort_ord(fst), msort_ord(snd))
  }
}

msort_ord(nums)
msort_ord(fruits)