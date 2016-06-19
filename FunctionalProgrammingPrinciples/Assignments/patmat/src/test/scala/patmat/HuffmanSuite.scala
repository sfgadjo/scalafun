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

  test("times"){
    new TestTrees{
      assert(times(List[Char]('a', 'b', 'a', 'b', 'c')) === List[(Char, Int)](('c', 1), ('b', 2), ('a', 2)))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("makeOrderedLeafList for a more random frequency table") {
    assert(makeOrderedLeafList(List(('c', 6), ('z', 1), ('a', 3))) === List(Leaf('z',1), Leaf('a',3), Leaf('c',6)))
  }



  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("singleton false case"){
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(singleton(leaflist) === false)
  }

  test("singleton true case"){
    val leaflist = List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3))
    assert(singleton(leaflist) === true)
  }

  test("singleton true case with nested forks"){
    val leaflist = List(Fork(Fork(Leaf('z',1),Leaf('a', 1), List('z','a', 't', 'e'), 4),Leaf('t',2),List('e', 't'),3))
    assert(singleton(leaflist) === true)
  }

  test("until"){
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val expected = List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7))
    assert(until(singleton, combine)(leaflist) === expected)
  }

  test("singleton with single leave"){
    val leaflist = List(Leaf('e', 1))
    assert(singleton(leaflist) === false)
  }

  test("create codetree"){
    val charlist = List('t','x','x','e','x','t','x')
    val expected = Fork(
      Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)
      ,Leaf('x',4),List('e', 't', 'x'),7)
    assert(createCodeTree(charlist) === expected)
  }

//  test("create codetree 2"){
//    var charlist = List('a','a','a','a','a','a','a','a','b','b','b','c', 'd', 'e', 'f', 'g', 'h')
//    assert(createCodeTree(charlist) === Nil)
//    val x=
//      Fork(
//        Fork(
//          Fork(
//            Leaf(c,1),Leaf(d,1),List(c, d),2), Fork(Leaf(e,1),Leaf(f,1),List(e, f),2),List(c, d, e, f),4),Fork(Fork(Leaf(g,1),Leaf(h,1),List(g, h),2),Fork(Leaf(b,3),Leaf(a,8),List(b, a),11),List(g, h, b, a),13),List(c, d, e, f, g, h, b, a),17)
//  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

//  test("code for char"){
//    val charlist = List('t','x','x','e','x','t','x', 'h','k', 'm','n','n','h','i')
//    var tree = createCodeTree(charlist)
//    println(tree)
//    assert(getCodeForChar('m', tree) == Nil)
//  }
//
  test("decoded french secret"){
    assert(decodedSecret === List[Char]())
  }
}
