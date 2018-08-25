package model

import model.data.Node
import util.QuickDiff

trait NodeQuickDiff extends QuickDiff[data.Node] {
  override def diffId(t: Node): Any = t.uuid
}
