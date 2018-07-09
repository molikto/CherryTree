package model.data


trait SpecialCharTrait extends Enumeration {
  type SpecialChar = Value

  // LATER mmm... really? https://en.wikipedia.org/wiki/Private_Use_Areas
  private[data] val SpecialCharStart = 0xF0000

  private[data] def createSpecialChar(id: Int) = apply(id)


  val EmphasisStart = Value
  val EmphasisEnd = Value
  val StrongStart = Value
  val StrongEnd = Value
  val StrikeThroughStart = Value
  val StrikeThroughEnd = Value
  val LinkStart = Value
  val LinkContentEnd = Value
  val LinkUrlEnd = Value
  val LinkTitleEnd = Value
  val ImageStart = Value
  val ImageContentEnd = Value
  val ImageUrlEnd = Value
  val ImageTitleEnd = Value
  val CodeStart = Value
  val CodeEnd = Value
  val LaTeXStart = Value
  val LaTeXEnd = Value

}

object SpecialChar {

  def apply(id: Int): SpecialChar = createSpecialChar(id)

  val formatted = Seq(
    (EmphasisStart, EmphasisEnd),
    (StrongStart, StrongEnd),
    (StrikeThroughStart, StrikeThroughEnd)
  )

  val linked = Seq(
    (LinkStart, LinkContentEnd, LinkUrlEnd, LinkTitleEnd),
    (ImageStart, ImageContentEnd, ImageUrlEnd, ImageTitleEnd)
  )

  val coded = Seq(
    (CodeStart, CodeEnd),
    (LaTeXStart, LaTeXEnd)
  )

  //** from inner to outter
  // also inner splits
  val surroundStartCodeInToOut: Seq[Unicode] = Seq(StrongStart, EmphasisStart, StrikeThroughStart, ImageStart, LinkStart).map(a => Unicode(a))
  val surroundStartCodeNotSplit: Seq[Unicode] = Seq(ImageStart, LinkStart).map(a => Unicode(a))

  val starts = formatted.map(_._1) ++ linked.map(_._1) ++ coded.map(_._1)
  val ends = formatted.map(_._2) ++ linked.map(_._4) ++ coded.map(_._2)
}

class CodedModifiers(val start: SpecialChar, val end: SpecialChar)

