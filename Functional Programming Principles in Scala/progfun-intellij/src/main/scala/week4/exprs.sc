import week4._

def show(e: Expr): String = e match {
  case Number(x) => x.toString
  case Sum(l, r) => show(l) + " + " + show(r)
  case Var(x) => x
  case Prod(l, r) => (l match {
    case sum: Sum => "(" + show(sum) + ")"
    case x: Expr => show(x)
  }) + "*" + (r match {
    case sum: Sum => "(" + show(sum) + ")"
    case x: Expr => show(x)
  })
}