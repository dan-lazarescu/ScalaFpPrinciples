//possible last implementation
def last[T](xs: List[T]): T = xs match {
  case List() => throw Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

//possible init implementation
def init[T](xs: List[T]): List[T] = xs match
  case List() => throw Error("last of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)

//  possible concat implementation
extension[T](xs: List[T])
  def ++ (ys: List[T]): List[T] = xs match
    case Nil => ys
    case x :: xs1 => x :: (xs1 ++ ys)

//possible implementation of reverse
extension[T](xs: List[T])
  def reverse: List[T] = xs match
    case Nil => Nil
    case y :: ys => ys.reverse ++ List(y)

//my implementation
def removeAt[T](n: Int, xs: List[T]): List[T] =
  def loop(xs: List[T], count: Int): List[T] =
    if count == n then xs.tail
    else xs.head :: loop(xs.tail, count + 1)
  loop(xs, 0)

def removeAtCourse[T](n: Int, xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case y :: ys =>
    if n == 0 then ys
    else removeAtCourse(n-1, ys)
}

def flatten(xs: Any): List[Any] = xs match
  case Nil => Nil
  case y :: ys => flatten(y) ++ flatten(ys)
  case _ => xs :: Nil

extension [T](xs: List[T])
  def splitAt(n: Int) = (xs.take(n), xs.drop(n))

def msort(xs: List[Int]): List[Int] =
  val n = xs.length / 2
  if n == 0 then xs
  else
    def merge(xs: List[Int], ys: List[Int]): List[Int] =
      if xs.isEmpty then ys
      else if ys.isEmpty then xs
      else if xs.head < ys.head then xs.head :: merge(xs.tail, ys)
      else ys.head :: merge(xs, ys.tail)
    val (fst, snd) = xs.splitAt(n)
    merge(msort(fst), msort(snd))

//merge with pattern matching
def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match
  case (Nil, ys) => ys
  case (xs, Nil) => xs
  case (x :: xs1, y :: ys1) =>
    if x < y then x :: merge(xs1, ys)
    else y :: merge(xs, ys1)

//msort(List(8, 5, 2, 10, 1, 4))

/*making sort polymorphic by passing the comparison op as additional parameter
so we can sort List of any type
def msort[T](xs: List[T])(lt: (T, T) => Boolean) =
  ???
  merge(msort(fst)(lt), msort(snd)(lt))
//  merge will be adapted too -> if lt(x, y) then ...
val xs = List(-5, 6, 3, 2, 7)
val fruits = List("apple", "pear", "orange", "pineapple")
msort(xs)((x, y => x < y))
msort(fruits)((x, y) => x.compareTo(y) < 0)
*/

//extension [T](xs: List[T])
//  def mapp[U](f: T => U): List[U] = xs match
//    case Nil => xs
//    case x :: xs1 => f(x) :: mapp(f, xs1)

def squareList(xs: List[Int]): List[Int] = xs match
  case Nil => Nil
  case y :: ys => y * y :: squareList(ys)

def sqList(xs: List[Int]): List[Int] = xs.map(x => x * x)
def posElems(xs: List[Int]): List[Int] = xs.filter(x => x > 0)

val nums = List(1, 2, 3, 4, 5, 6)
nums.partition(x => x % 2 != 0) // (List(1, 3, 5), List(2, 4, 6))
nums.span(x => x % 2 != 0) // (List(1), List(2, 3, 4, 5, 6))

def pack[T](xs: List[T]): List[List[T]] = xs match
  case Nil => Nil
  case x :: xs1 =>
//    xs.takeWhile(y => y == x) :: pack(xs1.dropWhile(y => y == x))
    val (more, rest) = xs1.span(y => y == x)
    (x :: more) :: pack(rest)

def encode[T](xs: List[T]): List[(T, Int)] = pack(xs).map(x => (x.head, x.length))

encode(List("a", "a", "a", "b", "c", "c",  "d", "a"))
pack(List("a", "a", "a", "b", "c", "c",  "d", "a"))

//REDUCE LEFT -> inserts a given binary op between adj elems of a list
//List(x1, ..., xn).reduceLeft(op) = x1.op(x2).....op(xn)
def sum(xs: List[Int])     = (0 :: xs).reduceLeft((x, y) => x + y) //or (_ + _)
def product(xs: List[Int]) = (1 :: xs).reduceLeft((x, y) => x * y) //or (_ * _)

//foldLeft is like reduceLeft but takes an accumulator z as additional parameter
def sum2(xs: List[Int])     = xs.foldLeft(0)(_ + _)
def product2(xs: List[Int]) = xs.foldLeft(1)(_ * _)

def reverseFold[T](xs: List[T]): List[T] = xs.foldLeft(List[T]())((xs, x) => x :: xs)
reverseFold(List("a", "a", "a", "b", "c", "c",  "d", "a"))

def foldRight[U](z: U)(op: (T, U) => U): U = this match
  case Nil => z
  case x :: xs => op(x, xs.foldRight(z)(op))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  xs.foldRight(List[U]())((y, ys) => f(y) :: ys)

def lengthFun[T](xs: List[T]): Int =
  xs.foldRight(0)((x, n) => n + 1)

lengthFun(List("a", "a", "a", "b", "c", "c",  "d", "a"))