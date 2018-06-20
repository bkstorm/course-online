package week4

trait List[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  override def isEmpty = true

  override def head = throw new NoSuchElementException("Nil.tail")

  override def tail = throw new NoSuchElementException("Nil.tail")
}

object List {
  def get[T](index: Int, list: List[T]): T = {
    if (index < 0) {
      throw new IndexOutOfBoundsException
    } else if (index == 0) {
      list.head
    } else {
      get(index - 1, list.tail)
    }
  }

  def apply[T]: List[T] = new Nil

  def apply[T](elem1: T): List[T] = new Cons[T](elem1, new Nil[T])

  def apply[T](elem1: T, elem2: T): List[T] = new Cons(elem1, new Cons[T](elem2, new Nil[T]))

}