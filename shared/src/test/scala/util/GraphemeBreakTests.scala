package util

import model.data.{Unicode, UnicodeData}
import utest._


object GraphemeBreakTests extends TestSuite {

  val tests = Tests {

    val data = UnicodeData.graphemeTests.split("\n")
      .filter(_.nonEmpty).map(_.trim.split("÷").filter(!_.trim.isEmpty).toSeq.map(j => {
      val cps = j.split(" ").toSeq
        .filter(a => a != "" && a != "×").map(a => Integer.parseInt(a, 16)).toArray
      new String(cps, 0, cps.size)
    }))
    'graphemeBreak - {
      val a = System.currentTimeMillis()
      for (d <- data) {
        val a = Unicode(d.mkString("")).graphemes.toVector.map(_._2.iterator.toVector)
        val b = d.map(a => Unicode(a)).toSeq.map(_.iterator.toVector)
        assert(a == b)
      }
      println(s"in ${System.currentTimeMillis() - a}")
    }

    def testGraphemes(a: String, except: Int) = {
      val bigUnicode = Unicode(a)
      val res = bigUnicode.graphemes.toSeq
      for (a <- 0 until bigUnicode.size) {
        if (a != except) {
          val aa = bigUnicode.before(a).toSeq.reverse
          val bb = bigUnicode.after(a).toSeq
          assert(aa ++ bb == res)
        }
      }
    }

    'graphemeBeforeAndAfterAscii - {
      testGraphemes("kldsjafkldsjk23jk4j2342", -1)
    }
    'graphemeBeforeAndAfterAscii2 - {
      testGraphemes("what\u000d\u000awwhat", 5)
    }
  }
}
