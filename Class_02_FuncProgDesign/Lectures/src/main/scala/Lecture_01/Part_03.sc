trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate = f(self.generate).generate
  }
}

val randIntegers = new Generator[Int] {
  val rand = new java.util.Random
  override def generate: Int = rand.nextInt()
}
randIntegers.generate

val randBooleans = for (x <- randIntegers) yield x > 0
randBooleans.generate

def pairs[T, S](gen1: Generator[T], gen2: Generator[S]): Generator[(T, S)] =
  for (x <- gen1; y <- gen2) yield (x, y)

val randIntPairs = pairs(randIntegers, randIntegers)
randIntPairs.generate


def singleGen[T](x: T): Generator[T] = new Generator[T] {
  override def generate = x
}

def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- randIntegers) yield lo + x % (hi - lo)

def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)




def oneinN(n: Int): Generator[Boolean] =
  if (n <= 0) throw new IllegalArgumentException("n must be at least 1")
  else for (x <- randIntegers) yield x % n == 0

def randListLengthN(n: Int): Generator[List[Int]] = {
  val randOneInN = oneinN(n + 1)

  def emptyLists = singleGen(Nil)
  def nonEmptyLists = for {
    head <- randIntegers
    tail <- randLists
  } yield head :: tail

  def randLists: Generator[List[Int]] = for {
    isEmpty <- randOneInN
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

  randLists
}

val randLists = randListLengthN(4)
randLists.generate

val numTrials = 1000
(for (i <- 1 to numTrials) yield randLists.generate.length).sum / (1.0 * numTrials)



trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def randTree: Generator[Tree] = {

  def leafTree: Generator[Leaf] = for (x <- randIntegers) yield Leaf(x)
  def innerTree: Generator[Inner] = for {
    l <- randTree
    r <- randTree
  } yield Inner(l, r)

  def randTree: Generator[Tree] = for {
    isLeaf <- randBooleans
    tree <- if (isLeaf) leafTree else innerTree
  } yield tree

  randTree
}

randTree.generate



def test[T](r: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
  for (_ <- 0 until numTimes) {
    val value = r.generate
    assert(test(value), "Test failed for: " + value)
  }
  println("Test passed" + numTimes + " times")
}

//test(pairs(randLists, randLists)) {
//  case (xs, ys) => (xs ++ ys).length > xs.length
//}

// ScalaCheck / QuickCheck
forAll { (l1: List[Int], l2: List[Int]) =>
  l1.size + l2.size == (l1 ++ l2).size
}