package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a','b','d'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times hello world") {
//    println(times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')))
//    assertEquals(times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')),
//      List(('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1)))
  }

  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf list with diffrent weights") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
  }

  test("create a simple code tree".ignore) {
    val chars = List('a', 'a', 'b', 'c')
//    assert(trends.head.user == "a" || trends.head.user == "b")
    assertEquals(createCodeTree(chars), Fork(Fork(Leaf('b', 1), Leaf('c', 1), List('b', 'c'), 2), Leaf('a', 2), List('b', 'c', 'a'), 4))
  }

  test("decode frenchCode".ignore) {
    new TestTrees:
      assertEquals(decode(myCode, mySecret), "bac".toList)
      assertEquals(decode(frenchCode, secret), "ssddsdsa".toList)
  }

  test("encode something") {
    new TestTrees:
      assertEquals(encode(secCode)(List('a')), List(0))
      assertEquals(encode(secCode)(List('b')), List(1,0,0))
      assertEquals(encode(secCode)(List('c')), List(1,0,1,0))
      assertEquals(encode(secCode)(List('d')), List(1,0,1,1))
      assertEquals(encode(secCode)(List('e')), List(1,1,0,0))
      assertEquals(encode(secCode)(List('f')), List(1,1,0,1))
      assertEquals(encode(secCode)(List('g')), List(1,1,1,0))
      assertEquals(encode(secCode)(List('h')), List(1,1,1,1))
      assertEquals(encode(secCode)(List('a','b')), List(0, 1, 0, 0))
      assertEquals(encode(secCode)(List('a','c')), List(0, 1, 0, 1, 0))
      assertEquals(encode(secCode)(List('b', 'c')), List(1, 0, 0, 1, 0, 1, 0))
      assertEquals(encode(secCode)(List('b', 'a', 'c')), List(1, 0, 0, 0, 1, 0, 1, 0))
  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

  test("test code bits") {
    new TestTrees:
      assertEquals(codeBits(myCodeTable)('a'), List(1))
      assertEquals(codeBits(myCodeTable)('b'), List(0, 0))
      assertEquals(codeBits(myCodeTable)('c'), List(0, 1))
  }

  test("test mergeCodeTable") {
    new TestTrees:
      assertEquals(mergeCodeTables(myCodeTable, mySecCodeTable), List(('a', List(1)), ('b', List(0,0)), ('c', List(0,1)), ('d', List(1)), ('e', List(0))))
  }

  test("test convert code tree".ignore) {
    new TestTrees:
      assertEquals(convert(myCode), myCodeTable)
  }

  test("quickEncode something") {
    new TestTrees:
      assertEquals(quickEncode(myCode)(List('a')), List(1))
      assertEquals(quickEncode(myCode)(List('b')), List(0,0))
      assertEquals(quickEncode(myCode)(List('c')), List(0,1))
      assertEquals(quickEncode(myCode)(List('a','b')), List(1, 0, 0))
      assertEquals(quickEncode(myCode)(List('a','c')), List(1, 0, 1))
      assertEquals(quickEncode(myCode)(List('b', 'c')), List(0, 0, 0, 1))
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
