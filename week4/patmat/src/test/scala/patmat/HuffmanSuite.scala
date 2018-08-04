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


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(string2chars(\"hello, world\"))") {
    assert(times(string2Chars("hello, world")).toMap === List(
      ('h', 1),
      ('e', 1),
      ('l', 3),
      ('o', 2),
      (',', 1),
      (' ', 1),
      ('w', 1),
      ('r', 1),
      ('d', 1)
    ).toMap)
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine until singleton 01") {
    val chars = "fffffddddeee".toList
    val trees = makeOrderedLeafList(times(chars))
    val combinedTree = until(singleton, combine)(trees).head
    assert(combinedTree.weight === chars.size)

    val result = Fork(
      Leaf('f',5),
      Fork(
        Leaf('e',3),
        Leaf('d',4),
        List('e', 'd'),
        7
      ),
      List('f', 'e', 'd'),
      12
    )
    assert(combinedTree === result)
  }

  test("combine until singleton 02") {
    val chars = "ffffffdddee".toList
    val trees = makeOrderedLeafList(times(chars))
    val combinedTree = until(singleton, combine)(trees).head
    assert(combinedTree.weight === chars.size)

    val result = Fork(
      Fork(Leaf('e',2),Leaf('d',3),List('e', 'd'),5),
      Leaf('f',6),
      List('e', 'd', 'f'),
      11
    )
    assert(combinedTree === result)
  }

  test("decode with tree") {
    val tree = Fork(
      Fork(Leaf('e',2),Leaf('d',3),List('e', 'd'),5),
      Leaf('f',6),
      List('e', 'd', 'f'),
      11
    )

    val ans = decode(tree, List(0, 0, 1, 1, 0, 1))
    assert(ans === List('e', 'f', 'f', 'd'))
  }

  test("decode secret") {
    val ans = decodedSecret.mkString
    assert(ans === "huffmanestcool")
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("convert codeTree to codeTable") {
    val tree = Fork(
      Fork(Leaf('e',2),Leaf('d',3),List('e', 'd'),5),
      Leaf('f',6),
      List('e', 'd', 'f'),
      11
    )
    val table = convert(tree)
    assert(table === List(
      ('e', List(0, 0)),
      ('d', List(0, 1)),
      ('f', List(1))
    ))
  }

  test("quick encode") {
    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) / 1e6 + "ms")
      result
    }


    val msg = ("huffmanestcool" * 100).toList
    val slow = time{encode(frenchCode)(msg)}
    val quick = time{quickEncode(frenchCode)(msg)}
    assert(slow === quick)
  }
}
