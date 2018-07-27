package command

import model.data.Unicode
import model.range.IntRange

trait Motion {
  def act(commandState: CommandState, content: model.data.Rich, count: Int, r: IntRange, char: Option[Unicode]) : Option[IntRange] = {
    this match {
      case rich: RichMotion =>
        rich.move(commandState, content, count, r, char).map(a => r.merge(a._1, a._2))
      case to: TextObject =>
        to.move(content, r, char)
    }
  }
}

trait RichMotion extends Motion {
  // -1 always exclude, 0 left edge, 1 always include
  protected def move(content: model.data.Rich, r: IntRange): (IntRange, Int) = throw new NotImplementedError("MMmm....")
  def move(commandState: CommandState, content: model.data.Rich, count: Int, r: IntRange, char: Option[Unicode]): Option[(IntRange, Int)] =
    Some((0 until count).foldLeft((r, 0)) { (rr, _) => move(content, rr._1) })
}

trait TextObject extends Motion {
  def move(content: model.data.Rich, a: IntRange, char: Option[Unicode]): Option[IntRange]
}
