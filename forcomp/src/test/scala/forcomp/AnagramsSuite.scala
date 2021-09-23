package forcomp

class AnagramsSuite extends munit.FunSuite:
  import Anagrams.*

  test("wordOccurrences: abcd (3pts)") {
    assertEquals(wordOccurrences("abcd"), List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert (3pts)") {
    assertEquals(wordOccurrences("Robert"), List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e (5pts)") {
    assertEquals(sentenceOccurrences(List("abcd", "e")), List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }


  test("dictionaryByOccurrences.get: eat (10pts)") {
    assertEquals(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet), Some(Set("ate", "eat", "tea")))
    assertEquals(dictionaryByOccurrences.get(List(('a', 1), ('c', 1), ('t', 1))).map(_.toSet), Some(Set("act", "cat")))
    assertEquals(dictionaryByOccurrences.get(List(('a', 1), ('b', 1), ('t', 1))).map(_.toSet), Some(Set("bat", "tab")))
    assertEquals(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('m', 1))).map(_.toSet), Some(Set("Mae")))
    assertEquals(dictionaryByOccurrences.get(List(('e', 1), ('m', 1))).map(_.toSet), Some(Set("em", "me")))
  }


  test("wordAnagrams married (2pts)") {
    assertEquals(wordAnagrams("married").toSet, Set("married", "admirer"))
  }

  test("wordAnagrams Linux (2pts)".ignore) {
    assertEquals(wordAnagrams("Linux").toSet, Set("Linux", "Lin", "nil"))
  }

  test("wordAnagrams player (2pts)") {
    assertEquals(wordAnagrams("player").toSet, Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r (10pts)") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assertEquals(subtract(lard, r), lad)
  }

  test("subtract: emn - em (10pts)") {
    val emn = List(('e', 1), ('m', 1), ('n', 1))
    val em = List(('e', 1), ('m', 1))
    val n = List(('n', 1))
    assertEquals(subtract(emn, em), n)
  }

  test("combinations: abba (8pts)") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assertEquals(combinations(abba).toSet, abbacomb.toSet)
  }

  test("combinations: abc (8pts)".ignore) {
    val abc = List(('a', 1), ('b', 1), ('c', 1))
    val abccomb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('c', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 1), ('c', 1)),
      List(('b', 1), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 1))
    )
    assertEquals(combinations(abc).toSet, abccomb.toSet)
  }

  test("combinations: aabbb (8pts)") {
    val abba = List(('a', 2), ('b', 3))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2)),
      List(('b', 3)),
      List(('a', 1), ('b', 3)),
      List(('a', 2), ('b', 3)),
    )
    assertEquals(combinations(abba).toSet, abbacomb.toSet)
  }

  test("combinations: Yesman (8pts)".ignore) {
    val abba = List(('a', 1), ('e', 1), ('m', 1), ('n', 1), ('s', 1), ('y', 1))
    val aemnsy = List(
      List(),
      List(('a', 1)),
      List(('e', 1)),
      List(('m', 1)),
      List(('n', 1)),
      List(('s', 1)),
      List(('y', 1)),
      List(('a', 1), ('e', 1)),
      List(('a', 1), ('m', 1)),
      List(('a', 1), ('n', 1)),
      List(('a', 1), ('s', 1)),
      List(('a', 1), ('y', 1)),
      List(('a', 1), ('e', 1), ('m', 1)),
      List(('a', 1), ('e', 1), ('n', 1)),
      List(('a', 1), ('e', 1), ('s', 1)),
      List(('a', 1), ('e', 1), ('y', 1)),
      List(('a', 1), ('e', 1), ('m', 1), ('n', 1)),
      List(('a', 1), ('e', 1), ('m', 1), ('s', 1)),
      List(('a', 1), ('e', 1), ('m', 1), ('y', 1)),
    )
    assertEquals(combinations(abba).toSet, aemnsy.toSet)
  }

  test("combinations: aabbb (8pts)".ignore) {
    val abba = List(('a', 2), ('b', 3))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2)),
      List(('b', 3)),
      List(('a', 1), ('b', 3)),
      List(('a', 2), ('b', 3)),
    )
    assertEquals(combinations(abba).toSet, abbacomb.toSet)
  }

  test("combinations: []") {
    val occurence = List()
    assertEquals(combinations(occurence), List(Nil))
  }

  test("sentence anagrams: [] (10pts)") {
    val sentence = List()
    assertEquals(sentenceAnagrams(sentence), List(Nil))
  }

  test("sentence anagrams: Linux rulez (10pts)".ignore) {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assertEquals(sentenceAnagrams(sentence).toSet, anas.toSet)
  }

  test("sentence anagrams: You olive (10pts)") {
    val sentence = List("You", "olive")
    val olive = List(
      List("I", "love", "you"),
      List("I", "you", "love"),
      List("you", "I", "love"),
      List("you", "love", "I"),
      List("love", "I", "you"),
      List("love", "you", "I"),
      List("You", "olive"),
      List("olive", "You")
    )
    assertEquals(sentenceAnagrams(sentence).toSet, olive.toSet)
  }

  test("sentence anagrams: Yes man (10pts)".ignore) {
    val sentence = List("Yes", "man")
    val yesman = List(
      List("en", "as", "my"),
      List("en", "my", "as"),
      List("man", "yes"),
      List("men", "say"),
      List("as", "en", "my"),
      List("as", "my", "en"),
      List("sane", "my"),
      List("Sean", "my"),
      List("my", "en", "as"),
      List("my", "as", "en"),
      List("my", "sane"),
      List("my", "Sean"),
      List("say", "men"),
      List("yes", "man")
    )
    assertEquals(sentenceAnagrams(sentence).toSet, yesman.toSet)
  }
  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
