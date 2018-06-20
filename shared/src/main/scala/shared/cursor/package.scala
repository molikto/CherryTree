package shared

package object cursor {
  // when used as a insertion point, Seq.empty is generally invalid, because the app will not allow deleting root
  type Node = Seq[Int]
}
