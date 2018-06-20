package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("twoElementsIntoEmpty") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == Math.min(a, b)
  }

  property("insertThenDeleteEmpty") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  property("getMinContinually") = forAll { h: H =>
    def getMin(h: H, mins: List[A]): List[A] = {
      if (isEmpty(h)) {
        mins
      } else {
        val min = findMin(h)
        getMin(deleteMin(h), mins :+ min)
      }
    }

    val mins = getMin(h, Nil)
    mins == mins.sorted
  }

  property("minMeld") = forAll { (h1: H, h2: H) =>
    val min = findMin(meld(h1, h2))
    min == findMin(h1) || min == findMin(h2)
  }

  property("minInsertDeleteMin") = forAll { (h1: H, h2: H) =>
    def getMin(h: H, mins: List[A]): List[A] = {
      if (isEmpty(h)) {
        mins
      } else {
        val min = findMin(h)
        getMin(deleteMin(h), mins :+ min)
      }
    }

    val meld1 = meld(h1, h2)
    val mins1 = getMin(meld1, Nil)
    val minH1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(minH1, h2))
    val mins2 = getMin(meld2, Nil)
    mins1 == mins2
  }

}
