package shared

package object range {


  trait IntRange {
    val start: Int
    val endInclusive: Int

    def size = endInclusive - start + 1

    def assertSize(): Unit = {
      assert(start >= 0 && endInclusive >= start)
    }
    def inside(cursor: Int)= cursor > start && cursor < endInclusive
    def touch(cursor: Int) = cursor >= start && cursor <= endInclusive
  }


  type Unicode = IntRange

  abstract sealed class Node {
  }

  object Node {
    object Self extends Node
    case class Childs(parent: cursor.Node,
      override val start: Int,
      override val endInclusive: Int) extends Node with IntRange
  }
}
