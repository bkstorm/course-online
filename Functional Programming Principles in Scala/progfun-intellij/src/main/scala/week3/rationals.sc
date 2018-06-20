println("Welcome to the Scala worksheet")
val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
(x + y).mul(y)

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  val numer = x / g

  val denom = y / g

  def + (that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def -(that: Rational) = this + -that

  def mul(that: Rational) = new Rational(numer * that.numer, denom * that.denom)

  def unary_- = new Rational(-numer, denom)

  def < (that: Rational) = numer * that.denom < denom * that.numer

  def max(that: Rational) = if (this < that) that else this

  override def toString = numer + "/" + denom
}