/*this is wrong: exponential complexity for new operations (new classes)
trait Expr:
  def isNumber: Boolean
  def isSum: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr

class Number(n: Int) extends Expr:
  def isNumber = true
  def isSum = false
  def numValue = n
  def leftOp = throw Error("Number.leftOp")
  def rightOp = throw Error("Number.rightOp")

class Sum(e1: Expr, e2: Expr) extends Expr:
  def isNumber = false
  def isSum = true
  def numValue = throw Error("Sum.numValue")
  def leftOp = e1
  def rightOp = e2
*/

trait Expr
//  def eval: Int = this match
//    case Number(n) => n
//    case Sum(e1, e2) => e1.eval + e2.eval

case class Numb(n: Int) extends Expr
case class Var(x: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match {
  case Numb(n) => n
//  case Var(x) => x
//  case Prod(e1, e2) => eval(e1) * eval(e2)
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

val expr = Sum(Numb(1), Numb(1))
eval(expr)

def show(e: Expr): String = e match
  case Numb(n) => n.toString
  case Var(x) => x
  case Prod(e1, e2) => s"${showP(e1)} * ${showP(e2)}"
  case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"

def showP(e: Expr): String = e match {
  case e: Sum => s"(${show(e)})"
  case _ => show(e)
}

show(expr)