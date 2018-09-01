package model.data

import model.data

import scala.collection.mutable.ArrayBuffer

class EncodedSeqParseException(msg: String) extends IllegalStateException(msg) {
}


private[model] class EncodedSeqWriter {

  override def toString: String = throw new IllegalStateException("Not allowed")

  private val sb = new ArrayBuffer[Any]()

  private var size = 0

  def put(a: SpecialChar): Unit = {
    sb.append(a)
    size += 1
  }

  def put(url: Unicode): Unit = {
    sb.append(url)
    size += url.size
  }

  def toEncodedSeq: EncodedSeq = EncodedSeq.compact(sb)
}

private[model] class EncodedSeqReader(a: EncodedSeq) {
  private var start = 0

  override def toString: String = throw new IllegalStateException("Not allowed")

  def debugString: String = a.toString

  def isEmpty: Boolean = start >= a.fragmentSize

  def eatOrNotSpecial(): Option[SpecialChar] = {
    val c = a.fragment(start)
    c match {
      case char: SpecialChar =>
        start += 1
        Some(char)
      case _ =>
        None
    }
  }

  def eat(a: SpecialChar) : Unit = {
    if (!eatOrFalse(a)) {
      throw new EncodedSeqParseException("!!")
    }
  }

  def eatOrFalse(s: SpecialChar): Boolean = {
    if (a.fragment(start) == s) {
      start += 1
      true
    } else {
      false
    }
  }

  def eatUntilSpecialChar(): Unicode = {
    a.fragment(start) match {
      case a: Unicode =>
        start += 1
        a.guessProp
      case _ => Unicode.empty
    }
  }

  def eatUntilAndDrop(b: SpecialChar): Unicode = {
    val a = eatUntilSpecialChar()
    if (!eatOrFalse(b)) {
      throw new EncodedSeqParseException(s"Expecting $b")
    }
    a
  }
}
