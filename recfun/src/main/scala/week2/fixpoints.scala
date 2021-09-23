package week2

object fixpoints extends App {
  import scala.math.abs

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double =
    println("first_guess: " + firstGuess)

    def iterate(guess: Double): Double =
      val next = f(guess)
      println(next)
      if isCloseEnough(guess, next) then next
      else iterate(next)
    iterate(firstGuess)

//  this loops forever
//  def sqrt(x: Double) = fixedPoint(y => x / y)(1.0)
  def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)
  sqrt(2)
//  stabilizing by averaging
  def averageDamp(f: Double => Double)(x: Double): Double =
    (x + f(x)) / 2

  def sqrtImp(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)
  sqrtImp(2)
}
