val fruit = List("apples", "oranges", "pears")
val empty = List()
val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

// Lists made from empty list Nil and construction operator :: ("cons")
val fruitUsingCons = "apples" :: ("oranges" :: ("pears" :: Nil))

// in Scala, operators ending in ":" associate to the right
val fruitUsingConsAssociative = "apples" :: "oranges" :: "pears" :: Nil

// operators ending in ":" are also seen as method calls of the right-hand operand
// This means that "::" is basically the prepend operation
val fruitUsingRightOperandMethod = Nil.::("pears").::("oranges").::("apples")



// insertion sort

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}

val nums = List(8, 4, 3, 9, 10, 2)
isort(nums)