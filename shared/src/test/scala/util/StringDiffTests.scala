package util


import model.data.{Unicode, UnicodeData}
import utest._
import util.diff.{Operation, OperationType}

object StringDiffTests extends TestSuite {

  val tests = Tests {

    val data = UnicodeData.graphemeTests.split("\n")
      .filter(_.nonEmpty).map(_.trim.split("÷").filter(!_.trim.isEmpty).toSeq.map(j => {
      val cps = j.split(" ").toSeq
        .filter(a => a != "" && a != "×").map(a => Integer.parseInt(a, 16)).toArray
      new String(cps, 0, cps.size)
    }))

    val str1 = Unicode(data.toSeq.flatten.mkString(""))
    val str2 = Unicode(data.toSet.toSeq.flatten.mkString(""))

    'strDiff - {
      val diff = str1.diff(str2)
      assert(model.operation.Unicode.apply(diff, str1) == str2)
    }

  }
}
