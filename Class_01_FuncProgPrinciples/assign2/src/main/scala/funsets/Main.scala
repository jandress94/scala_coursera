package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  val nums = (x: Int) => -11 < x && x < 11

  println(forall(nums, x => x < 100))
  println(forall(nums, x => x % 2 == 0))

  println(exists(nums, x => x < 5))
  println(exists(nums, x => x < 0))
  println(exists(nums, x => x < 3))

  printSet(nums)

  val squares = map(nums, x => x * x)
  printSet(squares)
}
