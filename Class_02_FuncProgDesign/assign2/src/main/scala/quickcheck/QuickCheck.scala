package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.Random

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll {a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll {(v1: Int, v2: Int) =>
    val h = insert(v1, insert(v2, empty))
    val min = if (v1 <= v2) v1 else v2
    findMin(h) == min
  }

  property("minLessThanOrEqual") = forAll {(h: H, v: Int) =>
    findMin(insert(v,h)) <= v
  }

  property("deleteMin1") = forAll {(v: Int) =>
    val h = insert(v, empty)
    isEmpty(deleteMin(h))
  }

  property("deleteMin2") = forAll{ (v1: Int, v2: Int) =>
    val (m1, m2) = if (v1 <= v2) (v1, v2) else (v2, v1)
    val h = insert(v1, insert(v2, empty))

    val out1 = findMin(h)
    val h1 = deleteMin(h)
    val out2 = findMin(h1)
    val h2 = deleteMin(h1)
    !isEmpty(h) && !isEmpty(h1) && isEmpty(h2) && (out1 == m1) && (out2 == m2)
  }

  property("deleteToEmptyResets") = forAll { (h: H, v: Int) =>
    def makeEmpty(h: H): H = if (isEmpty(h)) h else makeEmpty(deleteMin(h))
    val emptyHeap = makeEmpty(h)
    findMin(insert(v, emptyHeap)) == v
  }

  def getVals(h: H): List[Int] = {
    if (isEmpty(h)) List()
    else findMin(h) :: getVals(deleteMin(h))
  }

  def isSorted(h: H): Boolean = {
    if (isEmpty(h)) true
    else if (isEmpty(deleteMin(h))) true
    else {
      val min1 = findMin(h)
      val hMinus1 = deleteMin(h)
      val min2 = findMin(hMinus1)
      (min1 <= min2) && isSorted(hMinus1)
    }
  }

  property("getSorted") = forAll { h: H =>
    isSorted(h)
  }

  property("minOfMeld") = forAll{ (h1: H, h2: H) =>
    val melded = meld(h1, h2)

    (isEmpty(h1), isEmpty(h2)) match {
      case (true, true) => isEmpty(melded)
      case (true, false) => findMin(melded) == findMin(h2)
      case (false, true) => findMin(melded) == findMin(h1)
      case (false, false) =>
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        val min = if (m1 <= m2) m1 else m2
        findMin(meld(h1, h2)) == min
    }
  }

  property("repeatMin Delete") = forAll { h: H =>
    if (isEmpty(h)) true else {
      val min = findMin(h) - 1
      if (min >= findMin(h)) true
      else {
        val hWithNewMin = insert(min, insert(min, h))
        val m1 = findMin(hWithNewMin)
        val h1 = deleteMin(hWithNewMin)
        val m2 = findMin(h1)
        val h2 = deleteMin(h1)
        val m3 = findMin(h2)
        !isEmpty(hWithNewMin) && !isEmpty(h1) && !isEmpty(h2) && (m1 == min) && (m2 == min) && (m3 == findMin(h))
      }
    }
  }

  property("growBig") = {
    val numEx = 1000
    val rand = new Random()
    var h = empty
    for {
      _ <- 1 to numEx
    } h = insert(rand.nextInt(), h)

    val values = getVals(h)
    values == values.sorted && values.length == numEx
  }

  property("Arbitrary List") = forAll { l: List[Int] =>
    var h = empty
    for (v <- l) h = insert(v, h)

    val values = getVals(h)
    values == l.sorted
  }

}
