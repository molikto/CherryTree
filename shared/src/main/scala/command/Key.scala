package command

import model.data.Unicode
import util.GraphemeSplitter

// key is a unicode codepoint

import Key.KeySeq

case class Key(
  a: Key.V,
  shift: Boolean = false,
  alt: Boolean = false,
  control: Boolean = false,
  meta: Boolean = false) {

  def withAllModifers: Seq[KeySeq] = {
    import Key._
    val allTrue = copy(shift = true, alt = true, control = true, meta = true)
    Seq(this,
      Shift + this, Meta + this, Control + this, Alt + this,
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
  }
  case object Shift extends Modifier {
    def +(key: Key): Key = key.copy(shift = true)
  }
  case object Meta extends Modifier {
    def +(key: Key): Key = key.copy(meta = true)
  }
  case object Control extends Modifier {
    def +(key: Key): Key = key.copy(control = true)
  }
  case object Alt extends Modifier {
    def +(key: Key): Key = key.copy(alt = true)
  }

  sealed trait V {
    def withAllModifers: Seq[KeySeq] = Key(this).withAllModifers
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
  object Delete extends V

  case class Unknown(k: String) extends V // a key not yet defined here...

  /**
    * the value is combined with modifiers
    */
  case class Grapheme(a: Unicode) extends V

  def isUnicodeKey(s: String): Boolean = s.length == 1 ||
    GraphemeSplitter.nextBreak(s) == s.length

  // LATER is all keyboard layout like this????
  private val shifted = Unicode("~!@#$%^&*()_+{}:\"|><?QWERTYUIOPASDFGHJKLZXCVBNM").codePoints

  private def assciiKeyWithModifier(a: Int): Key = Key(Grapheme(Unicode(a)), shift = shifted.contains(a))

  implicit def defaultAsciiKeysToKeySeq(s: String): KeySeq = Unicode(s).codePoints.map(assciiKeyWithModifier)

  implicit def singleToKey(s: V): KeySeq = Seq(Key(s))


}

