package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("calculator (a = 2, b = 3)") {
    val e_a = Signal(Literal(2))
    val e_b = Signal(Literal(3))

    val sheet = Map("a" -> e_a, "b" -> e_b)
    val calc = Calculator.computeValues(sheet)
    assert(calc("a")() == 2.0)
    assert(calc("b")() == 3.0)
  }

  test("calc (b = 2*a") {
    val e_a = Var(Literal(2))
    val e_b = Signal(Times(Literal(2), Ref("a")))

    val sheet = Map("a" -> e_a, "b" -> e_b)
    val calc = Calculator.computeValues(sheet)
    assert(calc("a")() == 2.0)
    assert(calc("b")() == 4.0)

    e_a() = Literal(3)
    assert(calc("a")() == 3.0)
    assert(calc("b")() == 6.0)
  }

  test("self loop") {
    val e_a = Signal(Ref("a"))
    val sheet = Map("a" -> e_a)
    val calc = Calculator.computeValues(sheet)
    assert(calc("a")().isNaN)
  }

  test("two loop") {
    val e_a = Signal(Ref("b"))
    val e_b = Signal(Ref("a"))
    val sheet = Map("a" -> e_a, "b" -> e_b)
    val calc = Calculator.computeValues(sheet)
    assert(calc("a")().isNaN)
    assert(calc("b")().isNaN)
  }

}
