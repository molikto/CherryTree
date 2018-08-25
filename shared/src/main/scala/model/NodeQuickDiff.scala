package model

import model.data.Node
import util.QuickDiff

trait NodeQuickDiff extends QuickDiff[data.Node] {
  protected override def idEq(t: Node, b: Node): Boolean = t.uuid == b.uuid
}
