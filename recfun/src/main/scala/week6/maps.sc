//polynomial example; op withDefaultValue turns a map into a total function
class Polynom(nonZeroTerms: Map[Int, Double]):
//  auxiliary constructor for instantiating Polynoms with a var args parameters & without Map()
//  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  def terms = nonZeroTerms.withDefaultValue(0.0)

//  def + (other: Polynom): Polynom =
//    Polynom(terms ++ other.terms.map((exp, coeff) => (exp, coeff + terms(exp))))

  //ex: design another ver of + in terms of foldLeft
  def + (other: Polynom): Polynom =
    Polynom(other.terms.foldLeft(terms)(addTerm))
//my implementation
  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = terms.get(term._1) match
    case Some(exponent) => terms + (term._1 -> (terms(term._1) + term._2))
    case None => terms + term
//coursera implementation
  def addTerm2(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
    val (exp, coeff) = term
    terms + (exp -> (terms(exp) + coeff))

  //  override def toString: String = terms.toString -> exposes implementation with maps & is not friendly
  override def toString: String =
    val termStrings =
      for (exp, coeff) <- terms.toList.sorted.reverse
      yield
        val exponent = if exp == 0 then "" else s"x^$exp"
        s"$coeff$exponent"
    if terms.isEmpty then "0"
    else termStrings.mkString(" + ")

val x = Polynom(Map(0 -> 2, 1 -> -3, 2 -> 1))
val y = Polynom(Map(0 -> 3, 1 -> 4, 2 -> 2, 3 -> 1))
//val x = Polynom(0 -> 2, 1 -> -3, 2 -> 1)
//val y = Polynom(0 -> 3, 1 -> 4, 2 -> 2, 3 -> 1)
val z = Polynom(Map())
x + y