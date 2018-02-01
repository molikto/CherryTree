package shared

import shared.data.Node.Content
import shared.data.{Change, Node, Transaction}

import scala.util.Random

package object test {

  def randomContent(): Content = Random.nextLong() + " "

  def randomNonRootChild(root: Node): Node.Ref = {
    var c = randomChild(root)
    while (c == Node.Ref.root) {
      c = randomChild(root)
    }
    c
  }

  def randomChild(root: Node): Node.Ref = {
    var continue = true
    var node = Node.Ref.root
    while (continue) {
      val c = root(node)
      if (c.childs.isEmpty) {
        node = node.parent
      } else {
        node = node.withChild(Random.nextInt(c.childs.size))
      }
      continue = Random.nextInt(5) != 0
    }
    node
  }

  def randomSingleChangeTransaction(root: Node): Transaction = Transaction(Seq(randomChange(root)))

  def randomPoint(root: Node): Node.PointRef = {
    val n = randomChild(root)
    val c = root(n)
    val p = Random.nextInt(c.content.size + 1)
    Node.PointRef(n, p)
  }

  def randomSegment(root: Node): Option[Node.SegmentRef] = {
    val n = randomChild(root)
    val c = root(n)
    if (c.content.isEmpty) {
      None
    } else {
      val p = Random.nextInt(c.content.size)
      val j = Random.nextInt(c.content.size)
      val max = p max j
      val min = p min j
      Some(Node.SegmentRef(n, Node.Content.SegmentRef(min, max)))
    }
  }

  def randomChange(root: Node): Change = {
    Random.nextInt(10) match {
      case 0 | 1 | 2 | 3 =>
        val node = randomChild(root)
        val n = if (node == Node.Ref.root || Random.nextBoolean()) node.withChild(0)
                else if (Random.nextBoolean()) node.next
                else node
        Change.Node.Insert(n, Node.empty(Node.newId()).copy(content = randomContent()))
      case 4 if root.childs.nonEmpty =>
        Change.Node.Delete(randomNonRootChild(root))
      case 5 =>
        randomSegment(root) match {
          case Some(seg) =>
            Change.Content.Delete(seg)
          case _ =>
            Change.Content.Insert(randomPoint(root), randomContent())
        }
      case _ =>
        Change.Content.Insert(randomPoint(root), randomContent())
    }
  }
}
