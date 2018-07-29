package register

import model.{data, range}

sealed trait Registerable extends scala.AnyRef {

}

object Registerable {
  case class Node(a: Seq[data.Node], var from: Option[range.Node] = None, var needsClone: Boolean) extends Registerable
  case class Text(a: Seq[data.Text]) extends Registerable
  case class Unicode(a: data.Unicode) extends Registerable
}
