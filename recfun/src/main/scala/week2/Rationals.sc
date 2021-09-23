class Rational(x: Int, y: Int):
  require(y > 0, "denominator must be positive")
  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)
//  private val g = gcd(x, y)

//  def numer = x / g
//  def denom = y / g
  val numer = x / gcd(x.abs, y)
  val denom = y / gcd(x.abs, y)

  def this(x: Int) = this(x, 1)

  def add(r: Rational): Rational =
    Rational(numer * r.denom + r.numer * denom, denom * r.denom)

  def sub(r: Rational): Rational =
    Rational(numer * r.denom - r.numer * denom, denom * r.denom)

  def mul(r: Rational): Rational =
    Rational(numer * r.numer, denom * r.denom)

  def neg: Rational = Rational(-numer, denom)

  def less(that: Rational): Boolean =
    numer * that.denom < that.numer * denom

  def max(that: Rational): Rational =
    if this.less(that) then that else this

  override def toString: String = s"$numer/$denom"
end Rational

extension (r: Rational)
  infix def min(s: Rational): Rational = if s.less(r) then s else r
  def abs: Rational = Rational(r.numer.abs, r.denom)
  def + (y: Rational): Rational = r.add(y)
  def * (y: Rational): Rational = r.mul(y)
  def < (y: Rational): Boolean = r.less(y)
  def - (y: Rational): Rational = r.sub(y)

val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)
val a = Rational(3)
x.add(y).mul(z)
x.neg
x.sub(y).sub(z)
x.less(y)
y.max(z)