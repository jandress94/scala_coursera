val nums = List(2, -4, 5, 6, 3, 1)

// have to explicitly add the first elements because reduceLeft is not allowed to be called on empty lists
def sum(xs: List[Int]) = (0 :: xs).reduceLeft(_ + _)
def prod(xs: List[Int]) = (1 :: xs).reduceLeft(_ * _)

sum(nums)
prod(nums)

sum(List())

def sum2(xs: List[Int]) = xs.foldLeft(0)(_ + _)
def prod2(xs: List[Int]) = xs.foldLeft(1)(_ * _)

sum2(nums)
prod2(nums)