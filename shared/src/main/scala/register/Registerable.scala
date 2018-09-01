package register

import model.data.Node
import model.{data, range}

sealed trait Registerable extends scala.AnyRef {

  def isEmpty = false

}

object Registerable {
  case class Node(a: Seq[data.Node], var from: Option[range.Node] = None, var needsClone: Boolean) extends Registerable {
    override def isEmpty: Boolean = a.isEmpty

    override def equals(obj: scala.Any): Boolean = obj match {
      case n: Node => this eq n
      case _ => false
    }
  }
  case class Text(a: Seq[data.Text]) extends Registerable {
    override def isEmpty: Boolean = a.isEmpty
  }
  case class Unicode(a: data.Unicode) extends Registerable {
    override def isEmpty: Boolean = a.isEmpty
  }
}
