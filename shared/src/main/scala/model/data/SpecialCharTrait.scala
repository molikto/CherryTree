package model.data


trait SpecialCharTrait extends Enumeration {
  type SpecialChar = Value

  // LATER mmm... really? https://en.wikipedia.org/wiki/Private_Use_Areas
  private[data] val SpecialCharStart = 0xF0000

  private[data] def createSpecialChar(id: Int) = apply(id)


  val EmphasisStart, EmphasisEnd, 
    StrongStart, StrongEnd,
    StrikeThroughStart, StrikeThroughEnd,
    LinkStart, LinkContentEnd, LinkUrlEnd, LinkTitleEnd,
    ImageStart, ImageContentEnd, ImageUrlEnd, ImageTitleEnd,
    CodeStart, CodeEnd,
    LaTeXStart, LaTeXEnd:
    SpecialChar = Value

}

object SpecialChar {

  def apply(id: Int): SpecialChar = createSpecialChar(id)

  val formatted = Seq(
    FormattedModifiers(StrongStart, StrongEnd),
    FormattedModifiers(EmphasisStart, EmphasisEnd),
    FormattedModifiers(StrikeThroughStart, StrikeThroughEnd)
  )

  val linked = Seq(
    LinkedModifiers(LinkStart, LinkContentEnd, LinkUrlEnd, LinkTitleEnd),
    LinkedModifiers(ImageStart, ImageContentEnd, ImageUrlEnd, ImageTitleEnd)
  )

  val coded = Seq(
    CodedModifiers(LaTeXStart, LaTeXEnd),
    CodedModifiers(CodeStart, CodeEnd)
  )
  
  val all: Seq[DelimitedModifiers] = coded ++ formatted ++ linked
  
  val splittableOrdered: Seq[DelimitedModifiers] = formatted ++ linked
  val nonSplittableOrdered: Seq[DelimitedModifiers] = linked
  //** from inner to outer
  // also inner splits
  val surroundStartCodeInToOut: Seq[Unicode] = splittableOrdered.map(a => a.startUnicode)
  val surroundStartCodeNotSplit: Seq[Unicode] = nonSplittableOrdered.map(a => a.startUnicode)

  val starts: Seq[SpecialChar] = all.map(_.start)
  val ends: Seq[SpecialChar] = all.map(_.end)
}

sealed abstract class DelimitedModifiers {
  val start: SpecialChar
  val end: SpecialChar
  def  startUnicode = Unicode(start)
  def  endUnicode = Unicode(end)
}
case class CodedModifiers(start: SpecialChar, end: SpecialChar) extends DelimitedModifiers
case class FormattedModifiers(start: SpecialChar, end: SpecialChar) extends DelimitedModifiers
case class LinkedModifiers(start: SpecialChar, contentEnd: SpecialChar, urlEnd: SpecialChar, end: SpecialChar) extends DelimitedModifiers

