import java.io.{File, PrintWriter}

import scala.io.Source


def readDataFile(fn: String) = {
  val res = Source.fromFile(fn)
    .getLines().filter(a => a.trim.nonEmpty && !a.startsWith("#"))
    .toSeq
    .map(a => {
      val s1 = a.split(';')
      val range = s1.head.trim
      val cat = s1(1).split("#").head.trim
      val ran = if (range.contains("""..""")) {
        val pair = range.split("""\.\.""")
        Range.inclusive(Integer.parseInt(pair(0), 16), Integer.parseInt(pair(1), 16))
      } else {
        val i = Integer.parseInt(range, 16)
        Range.inclusive(i, i)
      }
      ran -> cat
    }).groupBy(_._2).mapValues(_.map(_._1))
  res
}

def assertNonOverlap(allRanges: Seq[Range]) = {
  for (i <- allRanges.indices) {
    val a = allRanges(i)
    for (j <- allRanges.drop(i + 1)) {
      assert(a.intersect(j).isEmpty)
    }
  }
}

def overlap(a: Seq[Range], b: Range): Boolean = a.exists(_.intersect(b).nonEmpty)
def overlap(a: Seq[Range], b: Seq[Range]): Boolean = a.exists(c => overlap(b, c))

val BreakPropertyMap = readDataFile("GraphemeBreakProperty.txt")

val BreakProperty = BreakPropertyMap.keys.toSeq

assertNonOverlap(BreakPropertyMap.values.flatten.toSeq)


val ExtendedPictographic = readDataFile("emoji-data.txt")("Extended_Pictographic")

assertNonOverlap(ExtendedPictographic)


val currentlyNotOverlap = BreakPropertyMap.filter(a => overlap(a._2, ExtendedPictographic)).keys

assert(currentlyNotOverlap.isEmpty)


val cats = BreakPropertyMap.updated("pExtended_Pictographic", ExtendedPictographic)

val rangeSorted = cats.toSeq.flatMap(a => a._2.map(k => (k, a._1))).sortBy(_._1.start)




def write(string: String, file: File) = {
  file.getAbsoluteFile.getParentFile.mkdirs()
  file.createNewFile()
  val writer = new PrintWriter(file)
  writer.write(string)
  writer.close()
}
println(s"here are ${rangeSorted.size} ranges")

write(s"""
   | object UnicodeGraphemeBreakData {
   |${cats.keys.zipWithIndex.map(a => s"val ${a._1} = ${a._2}").mkString("\n")}
   |val starts = Array(${rangeSorted.map(_._1.start).mkString(", ")})
   |val ends = Array(${rangeSorted.map(_._1.end).mkString(", ")})
   |val cats = Array(${rangeSorted.map(_._2).mkString(", ")})
   |
   |}
 """.stripMargin, new File("UnicodeGraphemeBreakData.scala"))


