package util

import model.data.{Unicode, UnicodeData}
import utest._

import scala.util.Random


object GraphemeBreakTests extends TestSuite {

  val tests = Tests {

    val data = UnicodeData.graphemeTests.split("\n")
      .filter(_.nonEmpty).map(_.trim.split("÷").filter(!_.trim.isEmpty).toSeq.map(j => {
      val cps = j.split(" ").toSeq
        .filter(a => a != "" && a != "×").map(a => Integer.parseInt(a, 16)).toArray
      new String(cps, 0, cps.size)
    }))

    def testGraphemes(a: String) = {
      val bigUnicode = Unicode(a)
      val res = bigUnicode.graphemes.toSeq
      for (a <- bigUnicode.after(0).map(_._1)) {
        val aa = bigUnicode.before(a).toSeq.reverse
        val bb = bigUnicode.after(a).toSeq
        assert(aa ++ bb == res)
      }
    }

    'graphemeBreak - {
      val a = System.currentTimeMillis()
      for (d <- data) {
        val a = Unicode(d.mkString("")).graphemes.toVector.map(_._2.iterator.toVector)
        val b = d.map(a => Unicode(a)).map(_.iterator.toVector)
        assert(a == b)
      }
      println(s"in ${System.currentTimeMillis() - a}")
      for (d <- data) {
        testGraphemes(d.mkString(""))
      }
      testGraphemes(data.flatten.mkString(""))
    }


    'graphemeBeforeAndAfterAscii - {
      testGraphemes("kldsjafkldsjk23jk4j2342")
    }
    'graphemeBeforeAndAfterAscii2 - {
      testGraphemes("what\u000d\u000awwhat")
    }


    'grapemeRandom - {
      for (_ <- 0 until 10000) {
        testGraphemes(Random.nextString(100))
      }
    }
  }
}
