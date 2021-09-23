
/*Lecture 2 - Currying + mapReduce

def sum(f: Int => Int): (Int, Int) => Int =
  def sumF(a: Int, b: Int): Int =
    if a > b then 0
    else f(a) + sumF(a + 1, b)
  sumF
//prev def is equivalent to this one
*/def sum(f: Int => Int)(a: Int, b: Int): Int =
  if a > b then 0 else f(a) + sum(f)(a + 1, b)

//def product(f: Int => Int)(a: Int, b: Int): Int =
//  if (a > b) 1 else f(a) * product(f)(a + 1, b)
//
//product(x => x * x)(1, 5)

//def factorial(n: Int) = product(x => x)(1, n)
//
//factorial(5)
//
//def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
//  def recur(a: Int): Int =
//    if (a > b) zero
//    else combine(f(a), recur(a + 1))
//  recur(a)
//}
//
//def sum(f: Int => Int) = mapReduce(f, (x, y) => x + y, 0)
//def product(f: Int => Int) = mapReduce(f, (x, y) => x * y, 1)
//
//sum(factorial)(1, 5)
//product(identity)(1,6)

//Lecture 3 - Fixed Points example
//import scala.math.abs
//
//val tolerance = 0.0001
//
//def isCloseEnough(x: Double, y: Double) =
//  abs((x - y) / x) < tolerance
//
//def fixedPoint(f: Double => Double)(firstGuess: Double): Double =
//  def iterate(guess: Double): Double =
//    val next = f(guess)
//    if isCloseEnough(guess, next) then next
//    else iterate(next)
//  iterate(firstGuess)
//
//def sqrt(x: Double) = fixedPoint(y => x / y)(1.0)
//
//sqrt(2)
