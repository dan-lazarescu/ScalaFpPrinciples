//an enum enumerates all the cases of an ADT(Algebraic Data Type and nothing else (no ...extends Expr)
enum Expr:
  case Var(s: String)
  case Number(n:Int)
  case Sum(e1: Expr, e2: Expr)
  case Prod(e1: Expr, e2: Expr)
import Expr.*

def show(e: Expr): String = e match
  case Expr.Var(x) => x
  case Expr.Number(n) => n.toString
  case Expr.Sum(a, b) => s"${show(a)} + ${show(b)}"
  case Expr.Prod(a, b) => s"${showP(a)} + ${showP(b)}"

def showP(e: Expr): String = e match
  case e: Expr.Sum => s"(${show(e)})"
  case _ => show(e)

enum DayOfWeek:
  case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

import DayOfWeek.*

def isWeekend(day: DayOfWeek) = day match
  case Saturday | Sunday => true
  case _ => false

isWeekend(Tuesday)
isWeekend(Saturday)

enum Direction(val dx: Int, val dy: Int):
  case Right extends Direction(1, 0)
  case Up extends Direction(0, 1)
  case Left extends Direction(-1, 0)
  case Down extends Direction(0, -1)

//  values is an imm array of comp obj enum that contains all Enum values Direction => values(1) = Up
//  ordinal - Right=0, Up=1, Left=2 etc => ordinal + 1 = 1 % 4 =1
  def leftTurn = Direction.values((ordinal + 1) % 4)
end Direction

val r = Direction.Right
val u = r.leftTurn
val v = (u.dx, u.dy)

//ADTS & Enums are useful for domain modelling tasks where you need to define
//large no of data types without attaching operations (methods), for ex
enum PaymentMethod:
  case CreditCard(kind: Card, holder: String, number: Long, expires: Date)
  case PayPal(email: String)
  case Cash

enum Card:
  case Visa, Mastercard, Amex

