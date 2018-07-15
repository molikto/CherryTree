package command

// key is a unicode codepoint

case class Key(a: Key.Single,
  shift: Boolean = false,
  command: Boolean = false,
  control: Boolean = false) {

  def +(a: Key.Modifier): Key = a.modify(this)
}

object Key {

  sealed trait Modifier {
    def modify(key: Key): Key
  }
  case object Shift extends Modifier {
    override def modify(key: Key): Key = key.copy(shift = true)
  }
  case object Command extends Modifier {
    override def modify(key: Key): Key = key.copy(command = true)
  }
  case object Control extends Modifier {
    override def modify(key: Key): Key = key.copy(control = true)
  }

  sealed trait Single {

  }

  case object Home extends Single
  case object End extends Single
  case object Left extends Single
  case object Right extends Single
  case object Up extends Single
  case object Down extends Single
  case object Enter extends Single
  case object PageDown extends Single
  case object PageUp extends Single
  case object Backspace extends Single

  case class CodePointSingle(a: Int) extends Single

  implicit def stringToKey(s: String): Key = {
    assert(s.codePointCount(0, s.size) == 1)
    Key(CodePointSingle(s.codePointAt(0)))
  }

  implicit def singleToKey(s: Single): Key = {
    Key(s)
  }
}

