package calculator

import calculator.Calculator.eval

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.mapValues(expr => Signal(eval(expr(), namedExpressions)))

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def loop(expr: Expr, references: Map[String, Signal[Expr]], names: Set[String] = Set()): Double = expr match {
      case l: Literal => l.v
      case r: Ref => if(names.contains(r.name)) Double.NaN else loop(getReferenceExpr(r.name, references), references, names + r.name)
      case p: Plus => loop(p.a, references, names) + loop(p.b, references, names)
      case m: Minus => loop(m.a, references, names) - loop(m.b, references, names)
      case t: Times => loop(t.a, references, names) * loop(t.b, references, names)
      case d: Divide => loop(d.a, references, names) / loop(d.b, references, names)
    }

    loop(expr, references)
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
