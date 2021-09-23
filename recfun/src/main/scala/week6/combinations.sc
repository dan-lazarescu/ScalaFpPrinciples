//to list all combinations of numbers x & y where x is drawn from 1..M and y is drawn from 1..N
//(1 to M).flatMap(x => (1 to N).map(y => (x, y)))
(1 to 10).flatMap(x => (1 to 10).map(y => (x, y)))

//scalar product of 2 vectors
def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
//  xs.zip(ys).map(xy => xy._1 * xy._2).sum
//  xs.zip(ys).map((x, y) => x * y).sum
  xs.zip(ys).map(_ * _).sum

/* Exercise: a no is prime if the only divisors of n are 1 and n itself
What is a high-level wat to write a test for primality of no? For once
value conciseness over efficiency */
//def isPrime(n: Int): Boolean = (2 until n).map(x => n % x != 0).reduceLeft(_ && _)
def isPrime(n: Int): Boolean = (2 until n).forall(n % _ != 0)

//ex: given n > 0 find all pairs of pozitive ints i, j with 1 <= j < i < n s.t. i + j is prime
def findPairs(n: Int): IndexedSeq[(Int, Int)] =
//  (1 until n).flatMap(x => (x + 1 until n).map(y => (x, y))).filter((x, y) => isPrime(x + y))
  (1 until n).map(i => (1 until i).map(j => (i, j))).flatten.filter((x, y) => isPrime(x + y))

//useful law: xs.flatMap(f) = xs.map(f).flatten
for i <- 1 until 8
  j <- 1 until i
  if isPrime(i + j)
  yield (i, j)

def scalarProdFor(xs: List[Double], ys: List[Double]): Double =
  (for (x, y) <- xs.zip(ys) yield x * y).sum

findPairs(4)
scalarProdFor(List(1.0,2.0), List(2.0,3.0)

type Occurrences = List[(Char, Int)]

def combinations(occurrences: Occurrences): List[Occurrences] =
  //    println(occurrences.toSet.subsets.map(_.toList).toList.sorted)
  def combs(occ: Occurrences): List[Occurrences] = occ match {
    case Nil => List(Nil)
    case x :: xs =>
      for
        o <- (1 to x._2).toList
        //          elem <- xs
        rest <- xs
      yield ((x._1, o), rest)
  }

