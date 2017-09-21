def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y * y :: squareList(ys)
}

def squareList2(xs: List[Int]): List[Int] =
  xs.map(x => x * x)

val nums = List(-3, -2, -1, 0, 1, 2, 3, 4, 5, -4, -5)

squareList(nums)
squareList2(nums)


def posElems(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
}

def posElems2(xs: List[Int]): List[Int] =
  xs.filter(x => x > 0)

posElems(nums)
posElems2(nums)

nums.filter(x => x > 0)
nums.filterNot(x => x > 0)
nums.partition(x => x > 0)

nums.takeWhile(x => x < 0)
nums.dropWhile(x => x < 0)
nums.span(x => x < 0)


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (firstElemReps, rest) = xs.span(y => y == x)
    firstElemReps :: pack(rest)
  }
}

val data = List("a", "a", "a", "b", "c", "c", "a")
pack(data)

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs).map(l => (l.head, l.length))

encode(data)