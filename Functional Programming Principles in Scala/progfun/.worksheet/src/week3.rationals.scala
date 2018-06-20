package week3

object rationals {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(78); 
  println("Welcome to the Scala worksheet");$skip(29); 
  val x = new Rational(1, 3);System.out.println("""x  : week3.Rational = """ + $show(x ));$skip(29); 
  val y = new Rational(5, 7);System.out.println("""y  : week3.Rational = """ + $show(y ));$skip(29); 
  val z = new Rational(3, 2);System.out.println("""z  : week3.Rational = """ + $show(z ));$skip(18); val res$0 = 
  x.add(y).mul(y);System.out.println("""res0: week3.Rational = """ + $show(res$0))}
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
