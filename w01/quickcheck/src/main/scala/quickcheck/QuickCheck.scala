package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // Hint #1:
  // If you insert any two elements into an empty heap, 
  // finding the minimum of the resulting heap should get 
  // the smallest of the two elements back.
  property("hint1") = forAll { (x: A, y: A) =>
    val h = insert(y, insert(x, empty))
    findMin(h) == min(x, y)
  }

  // Hint #2:
  // If you insert an element into an empty heap, 
  // then delete the minimum, the resulting heap should be empty.
  property("hint2") = forAll { x: A =>
    val h = deleteMin(insert(x, empty))
    isEmpty(h)
  }

  // Hint #3:  
  // Given any heap, you should get a sorted sequence 
  // of elements when continually finding and deleting minima. 
  // (Hint: recursion and helper functions are your friends.)
  property("hint3") = forAll { h: H =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val h1 = deleteMin(h)
        isEmpty(h1) || (findMin(h) <= findMin(h1) && isSorted(h1))
      }
    isSorted(h)
  }

  // Hint #4:
  // Finding a minimum of the melding of any two heaps 
  // should return a minimum of one or the other.   
  property("hint4") = forAll { (h1: H, h2: H) =>
    // Verifies the provided heaps are equal
    // by comparing all of their values.
    // assumption: the heaps are sorted
  	def areEqual(h1: H, h2: H): Boolean =
  	  if (isEmpty(h1) && isEmpty(h2)) true
  	  else findMin(h1) == findMin(h2) && areEqual(deleteMin(h1), deleteMin(h2))  	  
  	// Constructs two identical sorted heaps. Merge (meld) plays a central role here.
  	areEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
  
  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(value(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
