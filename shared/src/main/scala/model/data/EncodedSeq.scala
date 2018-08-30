package model.data
import boopickle.{PickleState, Pickler, UnpickleState}
import model.range.IntRange

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * normalized so that: there is no empty unicode, there is no unicode adjacent
  */
case class EncodedSeq private(seq: Seq[Any], unit: Unit) {



  if (model.debug_view) {
    assert({
      def test(): Boolean = {
        var previousIsString = false
        for (j <- seq) {
          j match {
            case s: SpecialChar =>
              previousIsString = false
            case a: Unicode =>
              if (previousIsString) {
                return false
              }
              previousIsString = true
            case _ => throw new IllegalStateException("not possible")
          }
        }
        true
      }
      test()
    })
  }


  override def toString: String = seq.mkString("^")

  private[data] def fragment(i: Int): Any = seq(i)
  private[data] def fragmentSize = seq.size
  lazy val size: Int = {
    var i = 0
    for (s <- seq) {
      s match {
        case _: SpecialChar => i += 1
        case u: Unicode => i += u.size
        case _ => throw new IllegalStateException("not possible")
      }
    }
    i
  }

  def isEmpty: Boolean = seq.isEmpty

  def insert(a: Int, b: EncodedSeq): EncodedSeq = slice(IntRange(0, a)) + b + slice(IntRange(a, size))

  def sliceAsUnicode(r: IntRange): Unicode = slice(r).singleUnicode

  def containsSpace: Boolean = seq.exists {
    case u: Unicode => u.containsSpace
    case _ => false
  }

  @inline
  def +(a: EncodedSeq): EncodedSeq =
    if (this.isEmpty) a
    else if (a.isEmpty) this
    else {
      (seq.last, a.seq.head) match {
        case (u: Unicode, k: Unicode) => EncodedSeq(seq.dropRight(1) ++ Seq(u + k) ++ a.seq.tail, Unit)
        case _ => EncodedSeq(seq ++ a.seq, Unit)
      }
    }

  def replace(r: IntRange, a: EncodedSeq): EncodedSeq = slice(IntRange(0, r.start))  + a + slice(IntRange(r.until, size))

  def delete(r: IntRange): EncodedSeq = slice(IntRange(0, r.start)) + slice(IntRange(r.until, size))

  def slice(r: IntRange): EncodedSeq = {
    if (r.start < 0 || r.until > size) throw new IllegalArgumentException("index out of bound")
    if (r.start == 0 && r.until == size) {
      this
    } else {
      // non empty
      var skiped = 0
      var index = 0
      val bf = new ArrayBuffer[Any]()
      while (skiped < r.start) {
        seq(index) match {
          case u: Unicode =>
            if (skiped + u.size > r.start) {
              val take = r.moveBy(-skiped)
              val j = if (take.until > u.size) take.copy(until = u.size) else take
              bf.append(u.slice(j))
            }
            skiped += u.size
            index += 1
          case a: SpecialChar =>
            skiped += 1
            index += 1
          case _ => throw new IllegalStateException("not possible")
        }
      }
      while (skiped < r.until) {
        seq(index) match {
          case u: Unicode =>
            if (skiped + u.size > r.until) {
              bf.append(u.slice(IntRange(0, r.until - skiped)))
            } else {
              bf.append(u)
            }
            skiped += u.size
            index += 1
          case a: SpecialChar =>
            bf.append(a)
            skiped += 1
            index += 1
          case _ => throw new IllegalStateException("not possible")
        }
      }
      EncodedSeq(bf, Unit)
    }
  }

  def surround(r: IntRange, left: EncodedSeq, right: EncodedSeq): EncodedSeq =
    slice(IntRange(0, r.start)) + left + slice(r) + right + slice(IntRange(r.until, size))

  def singleUnicodeOption: Option[Unicode] =
    if (seq.size == 1) {
      seq.head match {
        case u: Unicode => Some(u)
        case _ => None
      }
    } else {
      None
    }

  def singleUnicode: Unicode = if (seq.size == 1) {
    seq.head.asInstanceOf[Unicode]
  } else {
    throw new IllegalArgumentException("not single special")
  }

  def singleSpecial: SpecialChar = if (seq.size == 1) {
    seq.head.asInstanceOf[SpecialChar]
  } else {
    throw new IllegalArgumentException("not single special")
  }
}

object EncodedSeq extends DataObject[EncodedSeq] {
  /**
    * from a seq of special char and unicode
    */
  private[data] def compact(a: Seq[Any]): EncodedSeq = {
    if (a.isEmpty) {
      empty
    } else if (a.size == 1) {
      a.head match {
        case s: SpecialChar => EncodedSeq(a, Unit)
        case u: Unicode => if (u.isEmpty) empty else EncodedSeq(a, Unit)
        case _ => throw new IllegalArgumentException("Not allowed")
      }
    } else {
      val bf = new ArrayBuffer[Any]()
      val sb = new StringBuilder()
      for (j <- a) {
        j match {
          case s: SpecialChar =>
            if (sb.nonEmpty) {
              bf.append(Unicode(sb.toString()))
              sb.setLength(0)
            }
            bf.append(s)
          case a: Unicode =>
            if (a.nonEmpty) sb.append(a.str)
          case _ => throw new IllegalStateException("not possible")
        }
      }
      if (sb.nonEmpty) {
        bf.append(Unicode(sb.toString()))
      }
      EncodedSeq(bf, Unit)
    }
  }

  private[model] def apply(a: Seq[SpecialChar]): EncodedSeq = {
    EncodedSeq(a, Unit)
  }
  private[model] def apply(a: String): EncodedSeq = {
    if (a.isEmpty) {
      empty
    } else {
      EncodedSeq(Unicode(a))
    }
  }

  private[model] def apply(a: Unicode): EncodedSeq = {
    if (a.isEmpty) empty
    else EncodedSeq(Seq(a), Unit)
  }
  private[model] def apply(a: SpecialChar): EncodedSeq = {
    EncodedSeq(Seq(a))
  }

  override def random(r: Random): EncodedSeq = throw new IllegalArgumentException("don't just create random ones")

  override val pickler: Pickler[EncodedSeq] = new Pickler[EncodedSeq] {
    override def pickle(obj: EncodedSeq)(implicit state: PickleState): Unit = {
      import state.enc._
      writeInt(obj.seq.size)
      for (j <- obj.seq) {
        j match {
          case s: SpecialChar =>
            writeInt(-s.id - 1)
          case a: Unicode =>
            writeString(a.str)
          case _ => throw new IllegalStateException("not possible")
        }
      }
    }

    override def unpickle(implicit state: UnpickleState): EncodedSeq = {
      import state.dec._
      val bf = new ArrayBuffer[Any]()
      var previousIsString = false
      val size = readInt
      for (_ <- 0 until size) {
        val a = readInt
        if (a < 0) {
          previousIsString = false
          bf.append(SpecialChar(- a - 1))
        } else {
          assert(!previousIsString)
          previousIsString = true
          bf.append(Unicode(readString(a)))
        }
      }
      EncodedSeq(bf, Unit)
    }
  }

  val empty: EncodedSeq = EncodedSeq(Seq.empty)
}
