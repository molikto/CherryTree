package command

import model.data.Unicode
import util.GraphemeSplitter

import scala.collection.mutable.ArrayBuffer

// key is a unicode codepoint

import Key.KeySeq

case class Key(
  a: Key.V,
  shift: Boolean = false,
  alt: Boolean = false,
  control: Boolean = false,
  meta: Boolean = false) {

  def isModifier: Boolean = a match {
    case m: Key.Modifier => true
    case _ => false
  }

  def modKey(a: Boolean): Key = if (model.isMac) this.copy(meta = true) else this.copy(control = true)

  override def toString: String = {
    val sb = new ArrayBuffer[String]()
    if (shift) sb.append("Shift")
    if (control) sb.append("Ctrl")
    if (alt) sb.append("Alt")
    if (meta) sb.append("Meta")
    sb.append(a.toString)
    sb.mkString("+")
  }

  def withAllModifiers: Seq[KeySeq] = {
    import Key._
    val allTrue = copy(shift = true, alt = true, control = true, meta = true)
    Seq(this,
      Shift + this, Meta + this, Ctrl + this, Alt + this,
      copy(shift = true, meta = true), copy(shift = true, control = true), copy(shift = true, meta = true),
      copy(alt = true, control = true), copy(alt = true, meta = true), copy(control = true, meta = true),
      allTrue.copy(shift = false), allTrue.copy(alt = false), allTrue.copy(control = false), allTrue.copy(meta = false),
      allTrue).map(a => Seq(a))
  }
}

object Key {
  type KeySeq = Seq[Key]

  sealed trait Modifier extends V {
    def +(key: Key): Key
    def +(a: String): KeySeq = defaultAsciiKeysToKeySeq(a).map(this + _)
    def +(a: V): KeySeq = Seq(this + Key(a))
  }
  case object Shift extends Modifier {
    def +(key: Key): Key = key.copy(shift = true)
  }
  case object Meta extends Modifier {
    def +(key: Key): Key = key.copy(meta = true)
  }
  case object Ctrl extends Modifier {
    def +(key: Key): Key = key.copy(control = true)
  }
  case object Alt extends Modifier {
    def +(key: Key): Key = key.copy(alt = true)
  }

  val ModKey: Modifier = if (model.isMac) Meta else Ctrl

  sealed trait V {
    def withAllModifiers: Seq[KeySeq] = Key(this).withAllModifiers
  }

  case object Home extends V
  case object End extends V
  case object Left extends V
  case object Right extends V
  case object Up extends V
  case object Down extends V
  case object Enter extends V
  case object PageDown extends V
  case object PageUp extends V
  case object Backspace extends V
  case object Tab extends V
  case object Escape extends V
  case object Delete extends V

  case class Unknown(k: String) extends V // a key not yet defined here...

  /**
    * the value is combined with modifiers
    */
  case class Grapheme(a: Unicode) extends V {
    override def toString: String = a.str
  }
  object Grapheme {
    def apply(a: String): Grapheme = Grapheme(Unicode(a))
  }

  def isUnicodeKey(s: String): Boolean = s.length == 1 ||
    GraphemeSplitter.nextBreak(s) == s.length

  // LATER is all keyboard layout like this??
  private val shifted = Unicode("~!@#$%^&*()_+{}:\"|><?QWERTYUIOPASDFGHJKLZXCVBNM").toSeq

  private def assciiKeyWithModifier(a: Int): Key = Key(Grapheme(Unicode(a)), shift = shifted.contains(a))

  implicit def defaultAsciiKeysToKeySeq(s: String): KeySeq = Unicode(s).toSeq.map(assciiKeyWithModifier)

  implicit def singleToKey(s: V): KeySeq = s match {
    case Grapheme(u) if u.size == 1 => Seq(Key(s, shift = shifted.contains(u(0))))
    case _ => Seq(Key(s))
  }


}

