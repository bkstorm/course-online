package week3

object rationals {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val x = new Rational(1, 3)                      //> x  : week3.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : week3.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : week3.Rational = 3/2
  x.add(y).mul(y)                                 //> res0: week3.Rational = 110/147
}

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def subtract(that: Rational) = add(that.neg)

  def mul(that: Rational) = new Rational(numer * that.numer, denom * that.denom)

  def neg = new Rational(-numer, denom)

  override def toString = numer + "/" + denom
}