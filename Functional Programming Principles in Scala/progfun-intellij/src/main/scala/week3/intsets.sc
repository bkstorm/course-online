val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4

val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
val b: Array[IntSet] = a
b(0) = Empty
val s: NonEmpty = a(0)


abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def union(other: IntSet): IntSet

}

object Empty extends IntSet {
  def contains(x: Int) = false

  def incl(x: Int) = new NonEmpty(x, Empty, Empty)

  def union(other: IntSet): IntSet = other

  override def toString: String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int) = if (x < elem) {
    left contains x
  } else if (x > elem) {
    right contains x
  } else {
    true
  }

  override def incl(x: Int) = if (x < elem) {
    new NonEmpty(elem, left incl x, right)
  } else if (x > elem) {
    new NonEmpty(elem, left, right incl x)
  } else {
    this
  }

  override def union(other: IntSet): IntSet = ((left union right) union other) incl elem

  override def toString: String = "{" + left + elem + right + "}"
}