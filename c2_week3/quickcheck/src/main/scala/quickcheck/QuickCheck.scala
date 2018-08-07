package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.immutable

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      i <- arbitrary[Int]
      h <- oneOf(const(this.empty), genHeap)
    } yield this.insert(i, h)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, finding the
  // minimum of the resulting heap should get the smallest of the two
  // elements back.
  property("two elements") = forAll { (a: Int, b: Int) =>
    val h2 = insert(b, insert(a, empty))
    findMin(h2) == a.min(b)
  }

  // If you insert an element into an empty heap, then delete the minimum,
  // the resulting heap should be empty.
  property("delete min") = forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val hd = deleteMin(h1)
    isEmpty(hd)
  }

  def consume(heap: H): List[Int] = {
    if (isEmpty(heap)) Nil
    else findMin(heap) :: consume(deleteMin(heap))
  }

  // Given any heap, you should get a sorted sequence of elements when
  // continually finding and deleting minima. (Hint: recursion and helper
  // functions are your friends.)
  property("consume heap") = forAll { (h: H) =>
    val elements = consume(h)
    val sorted = elements.sorted
    elements == sorted
  }

  // Finding a minimum of the melding of any two heaps should return a
  // minimum of one or the other.
  property("meld heaps") = forAll { (h1: H, h2: H) =>
    val hm = meld(h1, h2)
    val meldMin = findMin(hm)
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    meldMin == min1.min(min2)
  }

  // API is
  //  def insert(x: A, h: H): H // the heap resulting from inserting x into h
  //  def meld(h1: H, h2: H): H // the heap resulting from merging h1 and h2
  //  def findMin(h: H): A // a minimum of the heap h
  //  def deleteMin(h: H): H // a heap resulting from deleting a minimum of h

  // Custom properties

  property("meld with empty") = forAll { (h: H) =>
    val hm1 = meld(h, empty)
    val hm2 = meld(h, empty)
    consume(h) == consume(hm1) && consume(h) == consume(hm2)
  }

  property("insert from one into another") = forAll { (h1: H, h2:H) =>
    val hm1 = meld(h1, h2)
    val hm2 = meld(deleteMin(h1), insert(findMin(h1), h2))
    val e1 = consume(hm1)
    val e2 = consume(hm2)
    val e =  (consume(h1) ::: consume(h2)).sorted
    e1 == e2 && e1 == e
  }



}
