package model.ot

import utest._

import scala.util.{Failure, Random, Success, Try}


object RebaseTests extends TestSuite {


  val rand = new Random()

  def randomContent() = rand.nextLong().toString

  def testNodeFromText(str: String): Node = {
    def rec2(left: Seq[Node], r: Seq[String]): (Seq[Node], Seq[String]) = {
      if (r.isEmpty) {
        (left, r)
      } else {
        val nContent = r.head
        val childs = r.tail.takeWhile(_.startsWith(" ")).map(_.drop(2))
        val r0 = r.tail.drop(childs.size)
        val n = Node(nContent, rec2(Seq.empty, childs)._1)
        rec2(left :+ n, r0)
      }
    }
    rec2(Seq.empty, str.split('\n'))._1.head
  }

  val tests = Tests {

    val randomText = "fdlksf lkasdfjklsa dfklds fjdsklf djsklf dslkfj slfjksla fjdsalfshgdsljfk"
    val node: Node = testNodeFromText(
      s""" $randomText
         |  0 $randomText
         |    0.0 $randomText
         |    0.1 $randomText
         |    0.2 $randomText
         |    0.3 $randomText
         |  1 $randomText
         |  2 $randomText
         |    2.0 $randomText
         |    2.1 $randomText
         |  3 $randomText
         |  4 $randomText
         |  """.stripMargin)

    val insert0 =
      NodeOps.insertNode(Seq(0), "insert0")
    val insert0c =
      NodeOps.insertNode(Seq(0), "insert0")
    val insert02 =
      NodeOps.insertNode(Seq(0,2), "insert02")
    val insert020 =
      NodeOps.insertNode(Seq(0, 2, 0), "insert020")
    val delete0 = NodeOps.deleteNode(Seq(0))
    val delete00 = NodeOps.deleteNode(Seq(0, 0))
    val delete00c = NodeOps.deleteNode(Seq(0, 0))
    val delete01 = NodeOps.deleteNode(Seq(0, 1))
    val delete02 = NodeOps.deleteNode(Seq(0, 2))
    val delete20 = NodeOps.deleteNode(Seq(2, 0))
    val insert0t = NodeOps.insertContent(Seq.empty, 4, randomContent())
    val insert0t1 = NodeOps.insertContent(Seq.empty, 6, randomContent())
    val insert0t1c = NodeOps.insertContent(Seq.empty, 6, randomContent())
    val insert03t = NodeOps.insertContent(Seq(0, 3), 20, randomContent())
    val insert02t = NodeOps.insertContent(Seq(0, 2), 20, randomContent())
    val delete0t = NodeOps.deleteContent(Seq.empty, 4, 3)
    val delete0t2 = NodeOps.deleteContent(Seq.empty, 5, 3)
    val delete0t3 = NodeOps.deleteContent(Seq.empty, 4, 4)
    val delete0t4 = NodeOps.deleteContent(Seq.empty, 5, 4)
    val delete0t5 = NodeOps.deleteContent(Seq.empty, 10, 4)
    val delete0t6 = NodeOps.deleteContent(Seq.empty, 0, 2)
    val delete0t7 = NodeOps.deleteContent(Seq.empty, 0, 10)
    val delete03t = NodeOps.deleteContent(Seq(0, 3), 20, 10)
    val delete02t = NodeOps.deleteContent(Seq(0, 2), 20, 10)

    val changes = Seq(insert0, insert0c, insert02, insert020,
      delete0, delete00, delete00c, delete01, delete02, delete20,
      insert0t, insert0t1, insert0t1c,
      insert03t, insert02t,
      delete0t, delete0t2, delete0t3, delete0t4, delete0t4, delete0t5, delete0t6, delete0t7,
      delete03t, delete02t)


    def assertRebase(a: Node.Operation, b: Node.Operation): Unit = {
      val debug = false
      if (debug) println(s"Change a: $a")
      if (debug) println(s"Change b: $b")
      ot.Node.rebase(a, b) match {
        case Rebased(s, (ap, bp)) =>
          if (debug) println(s"Rebase type s: $s")
          if (debug) println(s"Change a': $ap")
          if (debug) println(s"Change b': $bp")
          val app0 = ot.Node.apply(bp, ot.Node.apply(a, node))
          val app1 = ot.Node.apply(ap, ot.Node.apply(b, node))
          if (app0 == app1) {
            if (debug) println(s"App: $app0")
          } else {
            if (debug) println(s"App 0: $app0")
            if (debug) println(s"App 1: $app1")
          }
          assert(app0 == app1)
        case _ =>
      }
    }

    'simpleSymmetry - {
      for (a <- changes) {
        for (b <- changes) {
          val k = ot.Node.rebase(a, b)
          val j = ot.Node.rebase(b, a)
          (k, j) match {
            case (Rebased(ks, (kap, kbp)), Rebased(js, (jbp, jap))) =>
              if (!NodeOps.isAsymmetry(ks) && !NodeOps.isAsymmetry(js)) {
                assert(kap == jap)
                assert(kbp == jbp)
              }
          }
        }
      }
    }

    'simple - {
      for (c <- changes) {
        for (cc <- changes) {
          assertRebase(c, cc)
        }
      }
    }
    'squareSymmetry - {
      for (a1 <- changes) {
        for (a2 <- changes) {
          for (b1 <- changes) {
            for (b2 <- changes) {
              val a = Seq(a1, a2)
              val b = Seq(b1, b2)
              val k = ot.Node.rebase(a, b)
              val j = ot.Node.rebase(b, a)
              (k, j) match {
                case (Rebased(ks, (kap, kbp)), Rebased(js, (jbp, jap))) =>
                  // there are cases where asymmetry causes one path is conflict one is not
                  // for example
                  // left one is insert(4, len = 9) delete(0, 9)
                  // right is insert(4)
                  // if right is performed after left, it's insertion point is gone
                  // but otherwise it is ok
                  if (!NodeOps.isAsymmetry(ks) && !NodeOps.isAsymmetry(js)) {
                    assert(kap == jap)
                    assert(kbp == jbp)
                  }
              }
            }
          }
        }
      }
    }

    def assertRebaseSquare(a: Seq[Node.Operation], b: Seq[Node.Operation]): Unit = {
      val debug = false
      if (debug) println(s"Change a: $a")
      if (debug) println(s"Change b: $b")
      ot.Node.rebase(a, b) match {
        case Rebased(_, (ap, bp)) =>
          if (debug) println(s"Change a': $ap")
          if (debug) println(s"Change b': $bp")
          val app0 =  Try { ot.Node.apply(bp, ot.Node.apply(a, node)) }
          val app1 =  Try { ot.Node.apply(ap, ot.Node.apply(b, node)) }
          (app0, app1) match {
            case (Success(aaa), Success(bbb)) =>
              if (aaa == bbb) {
                if (debug) println(s"App: $aaa")
              } else {
                if (debug) println(s"App 0: $aaa")
                if (debug) println(s"App 1: $bbb")
              }
              assert(aaa == bbb)
            case (Failure(_), Failure(_)) =>
              Unit
            case (Success(s), Failure(e)) =>
              throw new IllegalArgumentException(s"a succeeded $s", e)
            case (Failure(e), Success(s)) =>
              throw new IllegalArgumentException(s"b succeeded$s", e)
          }
        case _ =>
      }
    }

    'square - {
      for (a1 <- changes) {
        for (a2 <- changes) {
          for (b1 <- changes) {
            for (b2 <- changes) {
              val a = Seq(a1, a2)
              val r1 = Try { ot.Node.apply(a, node) }
              val b = Seq(b1, b2)
              val r2 = Try { ot.Node.apply(b, node) }
              // for invalid sequences, rebasing might just make the sequence valid
              if (r1.isSuccess && r2.isSuccess) {
                assertRebaseSquare(a, b)
              }
            }
          }
        }
      }
    }
  }
}
