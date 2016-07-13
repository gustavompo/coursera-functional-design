package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  val maxVal = 86745
  lazy val genHeap: Gen[H] = for {
    int <- arbitrary[Int]
    maxIntV <- int % maxVal
    h <- oneOf(const(empty), genHeap)
  } yield insert(int, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("sortedDeletion") = forAll { (h1: H) =>
    val firstMin = findMin(h1)
    def findAllMin(heap:H, m:Int):Boolean = {
      if(isEmpty(heap)) return true
      val currentMin = findMin(heap)
      (currentMin >= m) && findAllMin(deleteMin(heap), currentMin)
    }
    findAllMin(deleteMin(h1), firstMin)
  }

  property("heapEquality") = forAll { (h1: H, h2: H) =>
    def isHeapEqual(h1: H, h2: H): Boolean = {
      def isEqualIter( status: Boolean, h1: H, h2: H): Boolean = {
        if (isEmpty(h1))
          if (isEmpty(h2))
            true
          else
            false
        else
          status && isEqualIter(findMin(h1) == findMin(h2), deleteMin(h1), deleteMin(h2))
      }

      isEqualIter( true, h1, h2)
    }

    isHeapEqual(meld(deleteMin(h1), insert(findMin(h1), h2)), meld(h1, h2))
  }

  property("insertMin") = forAll { (i1: Int, i2:Int, i3:Int) =>
    val min12 = math.min(i1, i2)
    val min123 = math.min(math.min(i1, i2), i3)

    val hA = insert(i1,insert(i2,empty))
    val hB = insert(i2,insert(i1,empty))
    val h3A = insert(i1,insert(i2,insert(i3, empty)))
    val h3B = insert(i3,insert(i1,insert(i2, empty)))

    val minHA = findMin(hA)
    val minHB = findMin(hB)
    val minH3A = findMin(h3A)
    val minH3B = findMin(h3B)

    (minHA == minHB) && (minHA == min12) && (minH3A == minH3B) && (min123 == minH3A)
  }

  property("deleteToEmpty") = forAll { (i: Int) =>
    val h = insert(i,empty)
    val emp = deleteMin(h)
    isEmpty(emp)
  }

  property("meldingMin") = forAll { (h1: H, h2: H) =>
    val min = math.min( findMin(h1), findMin(h2))
    val melded1 = meld(h1,h2)
    val melded2 = meld(h2, h1)
    min == findMin(melded1) && min == findMin(melded2)
  }
}