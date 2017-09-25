package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("List count") {
    val charCounts = times(string2Chars("Jim is cool"))
    assert(charCounts.contains(('J', 1)))
    assert(charCounts.contains(('i', 2)))
    assert(charCounts.contains(('m', 1)))
    assert(charCounts.contains((' ', 2)))
    assert(charCounts.contains(('s', 1)))
    assert(charCounts.contains(('c', 1)))
    assert(charCounts.contains(('o', 2)))
    assert(charCounts.contains(('l', 1)))
    assert(charCounts.size === 8)
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("Medium Length Text") {
    val text = string2Chars("Hello Jim, this is a medium length string.")
    val codeTree = createCodeTree(text)
    val encodedText = encode(codeTree)(text)
    println(encodedText)
    val decodedText = decode(codeTree, encodedText)
    println(decodedText)
  }

  test("Fast Encode same as normal") {
    val text = string2Chars("This is some testing text.")
    val codeTree = createCodeTree(text)
    val encodedText = encode(codeTree)(text)
    val quickEncodedText = quickEncode(codeTree)(text)

    assert(encodedText === quickEncodedText)

    println(decode(codeTree, quickEncodedText))
  }

}
