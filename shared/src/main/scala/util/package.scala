import java.nio.ByteBuffer

import model.data.Unicode

import scala.util.Random


package object util extends ObservablePropertyImplicits  {


  var debug_fpsView: String => Unit = null

  private var debug_fpsStartTime = 0L

  def fpsStart(): Unit = {
    debug_fpsStartTime = System.currentTimeMillis()
  }

  def fpsEnd(): Unit = {
    val time = System.currentTimeMillis() - debug_fpsStartTime
    if (debug_fpsView != null) {
      if (time > 1) {
        debug_fpsView(time.toString)
      }
    }
  }


  private val debugOn = false

  def isAscii(c: String): Boolean = Unicode(c).forall(isAscii)

  def isEnglishLetter(c: Int): Boolean =
    ('a' <= c && c <= 'z') ||
      ('A' <= c && c <= 'Z')

  def isEnglishLetter(a: String): Boolean = Unicode(a).forall(isEnglishLetter)

  def isAscii(codepoint: Int): Boolean = codepoint >= 0 && codepoint <= 0x7f

  def matchCommandSearch(str: String, term: String) = str.contains(term)

  def head[T](a: Iterator[T]): Option[T] = if (a.hasNext) Some(a.next()) else None
  def last[T](a: Iterator[T]): Option[T] = {
    var n: Option[T] = None
    while (a.hasNext) {
      n = Some(a.next())
    }
    n
  }


  def quickDiff(oldVal: String, newVal: String): (Int, Int, String) = {
    var start = 0
    var oldEnd = oldVal.length
    var newEnd = newVal.length
    while (start < newEnd && start < oldEnd && oldVal.codePointAt(start) == newVal.codePointAt(start)) {
      start += 1
    }
    while (oldEnd > start && newEnd > start && oldVal.codePointAt(oldEnd - 1) == newVal.codePointAt(newEnd - 1)) {
      oldEnd -= 1
      newEnd -= 1
    }
    val from = start
    val to = oldEnd
    val text = newVal.substring(start, newEnd)
    (from, to, text)
  }


  def debugged[T](a : => T)(implicit debug: Boolean = debugOn): T = {
    if (debug) {
      a match {
        case bb: ByteBuffer =>
          val l = bb.limit()
          val bs = (0 until l).map(_ => bb.get()).toArray
          println(s"[bytebuffer " + bs.mkString(", ") + "]")
          ByteBuffer.wrap(bs).order(bb.order()).asInstanceOf[T]
        case c =>
          println(s"[println $c]")
          c
      }
    } else {
      a
    }
  }

  def maxMin(a: Int, b: Int): (Int, Int) = {
    if (a > b) {
      (a, b)
    } else {
      (b, a)
    }
  }
}
