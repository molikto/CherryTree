package model.range


import api.{ApiError, ClientInit, ErrorT}
import model._
import model.data.DataObject
import utest._


object RangeTests extends TestSuite {

  val tests = Tests {

    'intRange - {
      val a = IntRange(12, 18)
      assert(a.moveBy(5) == IntRange(17, 23))
      assert(a.transformDeletingRangeAfterDeleted(IntRange(12, 19)).contains(IntRange(12, 13)))
      assert(a.transformDeletingRangeAfterDeleted(IntRange(11, 15)).contains(IntRange(11, 12)))
      assert(a.transformDeletingRangeAfterDeleted(IntRange(0, 3)).contains(IntRange(0, 3)))
      assert(a.transformDeletingRangeAfterDeleted(IntRange(12, 18)).isEmpty)
      assert(a.transformDeletingRangeAfterDeleted(IntRange(19, 22)).contains(IntRange(13, 16)))
      assert(a.transformDeletingRangeAfterDeleted(IntRange(0, 22)).contains(IntRange(0, 16)))
      assert(a.transformAfterDeleted(4).contains(4))
      assert(a.transformAfterDeleted(12).isEmpty)
      assert(a.transformAfterDeleted(14).isEmpty)
      assert(a.transformAfterDeleted(17).isEmpty)
      assert(a.transformAfterDeleted(18).contains(12))
      assert(!a.contains(1))
      assert(!a.contains(11))
      assert(a.contains(12))
      assert(a.contains(14))
      assert(a.contains(17))
      assert(!a.contains(18))
      assert(!a.contains(21))
      assert(!a.contains(IntRange(0, 2)))
      assert(!a.contains(IntRange(0, 14)))
      assert(!a.contains(IntRange(13, 19)))
      assert(!a.contains(IntRange(17, 19)))
      assert(!a.contains(IntRange(18, 22)))
      assert(a.contains(IntRange(12, 18)))
      assert(!a.overlap(IntRange(0, 2)))
      assert(a.overlap(IntRange(0, 14)))
      assert(a.overlap(IntRange(13, 19)))
      assert(a.overlap(IntRange(17, 19)))
      assert(!a.overlap(IntRange(18, 22)))
      assert(a.overlap(IntRange(12, 18)))
    }


    'node - {
      val a = Node(Seq(1, 2, 3), IntRange(5, 10))
      val sameLevel1 = Node(Seq(1, 2, 3), IntRange(2, 5))
      val sameLevel2 = Node(Seq(1, 2, 3), IntRange(4, 8))
      val sameLevel3 = Node(Seq(1, 2, 3), IntRange(7, 11))
      val sameLevel4 = Node(Seq(1, 2, 3), IntRange(10, 15))
      assert(a.sameParent(sameLevel1.start))
      assert(a.sameParent(sameLevel1.until))
      assert(a.contains(Seq(1, 2, 3, 7)))
      assert(!a.contains(Seq(1, 2, 3, 1)))
      assert(a.contains(Seq(1, 2, 3, 7, 9)))
      assert(!a.contains(Seq(1, 2, 3, 10, 9)))
      assert(a.contains(Seq(1, 2, 3, 9)))
      assert(!a.contains(Seq(1, 2, 3, 11)))
      assert(!a.contains(Seq(1, 2)))
      assert(!a.contains(Seq(1, 2, 3)))
      assert(!a.contains(Seq(1, 4)))
      assert(a.transformAfterDeleted(Seq(1, 2, 3, 9, 11)).isEmpty)
      assert(a.transformAfterDeleted(Seq(1, 2, 3)).contains(Seq(1, 2, 3)))
      assert(a.transformAfterDeleted(Seq(1, 2, 3, 10, 11)).contains(Seq(1, 2, 3, 5, 11)))
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3))).contains(Node(Seq(1, 2, 3))))
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3, 4))).contains(Node(Seq(1, 2, 3, 4))))
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3, 12))).contains(Node(Seq(1, 2, 3, 7))))
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3, 5))).isEmpty)
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3, 5), 5)).isEmpty)
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3, 5), 6)).contains(Node(Seq(1, 2, 3, 5))))
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3, 5, 4))).isEmpty)
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3, 5, 4, 7))).isEmpty)
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3, 5, 4, 7), 2)).isEmpty)
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3, 8))).isEmpty)
      assert(a.transformDeletingRangeAfterDeleted(Node(Seq(1, 2, 3, 9))).isEmpty)
    }
  }
}
