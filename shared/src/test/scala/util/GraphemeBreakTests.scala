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
      for (d <- data) {
        val a = Unicode(d.mkString("")).graphemes.toVector.map(_._2.iterator.toVector)
        val b = d.map(a => Unicode(a)).toVector.map(_.iterator.toVector)
        assert(a == b)
      }
    }

    'graphemeBeforeAndAfter - {
      val bigUnicode = Unicode("kldsjafkldsjk23jk4j2342")
      val res = bigUnicode.graphemes.toVector
      for (a <- 0 until bigUnicode.size) {
        assert(bigUnicode.before(a).toVector.reverse ++ bigUnicode.after(a).toVector == res)
      }
    }
  }
}
