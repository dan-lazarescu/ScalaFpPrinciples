package forcomp

import sun.font.TrueTypeFont

import scala.io.{Codec, Source}

object Anagrams extends AnagramsInterface:

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.groupBy(el => el.toLower).map((k, v) => (k, v.length)).toList.sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    def joinWords(sent: Sentence): Word = sent match
      case Nil => ""
      case x :: xs => x + joinWords(xs)
    wordOccurrences(joinWords(s))


  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)) match
    case Some(words) => words
    case None => List(word)

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */

  def combinations(occurrences: Occurrences): List[Occurrences] =
    //helper method to expand occurences like (a,2),(b,2)
    def expandList(initialOcc: Occurrences): Occurrences = initialOcc match
      case Nil => List()
      case x :: xs =>
        val result = (for
          occ <- (1 to x._2).toList
        yield (x._1, occ))
        result ::: expandList(xs)

    def combs(occ: Occurrences, acc: List[Set[(Char, Int)]]): List[Set[(Char, Int)]] = occ match
      case Nil => acc
      case x :: xs =>
        val result = (for
          elem <- xs
          if x._1 != elem._1 //eliminating comb like (a1, a2) or (b1, b2, b3 etc.)
        yield Set(x, elem) )

        val res = (for
          set <- acc
          if set.forall(p => p._1 != x._1) //only different chars are added to acc
        yield set + x)
        val newAcc = (acc ::: result ::: res).toSet.toList
        combs(xs, newAcc)

    val combList = combs(expandList(occurrences), List(Set()))
    combList.map(x => x.toList.sorted).sorted

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = y match
    case List() => x
    case y1 :: ys =>
      subtract(x.map(a => if a._1 == y1._1 then (a._1, a._2 - y1._2) else a), ys).filter((a, b) => b != 0)

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] =

    val sentOccurrences = sentenceOccurrences(sentence)
    val combs = combinations(sentOccurrences).filter(!_.isEmpty)
    println(combs)

    def countChars(sentence: Occurrences): Int =
      println("Counting chars: " + sentence.map(x => x._2).foldLeft(0)(_ + _))
      sentence.map(x => x._2).foldLeft(0)(_ + _)
    val noOfChars = countChars(sentOccurrences)

    def getWordList(sentence: List[Occurrences], acc: List[Sentence]): List[Sentence] = sentence match
      case List() => List(Nil)
      case x :: xs =>
        val solution = dictionaryByOccurrences.get(x) match
          case Some(words) =>
            println("The words are:" + words)
            val sentReduced = sentence.map(y => subtract(y, x)).toSet.toList.sorted
            println("REDUCED SENT: " + sentReduced)
            val posSolution = (for
              word <- words
              rest <- getWordList(sentReduced, acc)
//              if acc.forall(solution => !solution.contains(word))
            yield word :: rest)
            println("Intermediate result: " + posSolution)
//            getWordList(xs, solution)
            posSolution
          case None =>
            getWordList(xs, acc)
        println("SOLUTION: " + solution)
        solution

    getWordList(combs, List(Nil))


object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()
