class Coder(words: List[String]):
  val mnemonics = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  /** Maps a letter to the digit it represents */
  val charCode: Map[Char, Char] =
    for (digit, str) <- mnemonics
      letter <- str
    yield letter -> digit

  /** Maps a word to the digit string it can represent */
  def wordCode(word: String): String = word.toUpperCase.map(charCode)
//    for letter <- word
//    yield charCode(letter)

  /** Maps a digit string to all words in the dictionary that represent it */
  val wordsForNum: Map[String, List[String]] =
    words.groupBy(wordCode).withDefaultValue(Nil)

  /** All ways to encode a number as a list of words */
  def encode(number: String): Set[List[String]] =
    if number.isEmpty then Set(List())
    else
      for
        splitPoint <- (1 to number.length).toSet
        word <- wordsForNum(number.take(splitPoint))
        rest <- encode(number.drop(splitPoint))
      yield word :: rest

end Coder

val coder = Coder(List("AD", "BD", "DAC", "EBA", "FAC"))
coder.mnemonics
coder.charCode
coder.wordCode("AD")
coder.wordCode("AF")
coder.wordCode("BD")
coder.wordsForNum