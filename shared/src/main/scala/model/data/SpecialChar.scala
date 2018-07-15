package model.data


trait SpecialCharTrait extends Enumeration {
  type SpecialChar = Value

  // LATER mmm... this is really hacky really? https://en.wikipedia.org/wiki/Private_Use_Areas
  private[data] val SpecialCharStart = 0xF0000

  private[data] def createSpecialChar(id: Int) = apply(id)


  val EmphasisStart, EmphasisEnd,
    StrongStart, StrongEnd,
    StrikeThroughStart, StrikeThroughEnd,
    LinkStart, LinkEnd,
    ImageStart, ImageEnd,
    CodeStart, CodeEnd,
    LaTeXStart, LaTeXEnd,
    UrlAttribute, TitleAttribute:
    SpecialChar = Value

}

object SpecialChar {

  def apply(id: Int): SpecialChar = createSpecialChar(id)

  val Emphasis =
    Delimitation(EmphasisStart, EmphasisEnd)
  val Strong =
    Delimitation(StrongStart, StrongEnd)
  val StrikeThrough =
    Delimitation(StrikeThroughStart, StrikeThroughEnd)
  val formatted = Seq(
    Emphasis, Strong, StrikeThrough
  )

  val attributes = Seq(UrlAttribute, TitleAttribute)

  val Link =
    Delimitation(LinkStart, LinkEnd, attributes)

  val Image =
    Delimitation(ImageStart, ImageEnd, attributes)

  val linked = Seq(
    Link, Image
  )

  val Code =
    Delimitation(CodeStart, CodeEnd)

  val LaTeX =
    Delimitation(LaTeXStart, LaTeXEnd)

  val coded = Seq(Code, LaTeX)

  val all: Seq[Delimitation] = coded ++ formatted ++ linked

  val splittableOrdered: Seq[Delimitation] = formatted ++ linked
  val nonSplittableOrdered: Seq[Delimitation] = linked
  //** from inner to outer
  // also inner splits
  val surroundStartCodeInToOut: Seq[Unicode] = splittableOrdered.map(a => a.startUnicode)
  val surroundStartCodeNotSplit: Seq[Unicode] = nonSplittableOrdered.map(a => a.startUnicode)

  val starts: Seq[SpecialChar] = all.map(_.start)
  val ends: Seq[SpecialChar] = all.map(_.end)

  val startsEnds: Seq[SpecialChar] = starts ++ ends

  case class Delimitation(start: SpecialChar, end: SpecialChar, attributes: Seq[SpecialChar] = Seq.empty) {
    private[model] def startUnicode = Unicode(start)
    private[model] def endUnicode = Unicode(end)
  }

}

