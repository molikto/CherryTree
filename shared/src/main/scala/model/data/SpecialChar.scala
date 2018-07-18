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


  val attributes = Seq(UrlAttribute, TitleAttribute)

  val Link =
    Delimitation(LinkStart, LinkEnd, attributes)

  val Image =
    Delimitation(ImageStart, ImageEnd, attributes, isAtomic = true)


  val Code =
    Delimitation(CodeStart, CodeEnd)

  val LaTeX =
    Delimitation(LaTeXStart, LaTeXEnd, isAtomic = true)

  /**
    */
  val formatLike = Seq(
    Emphasis, Strong, StrikeThrough
  )

  val linkLike = Seq(Link)

  val imageLike = Seq(Image)


  val latexLike = Seq(LaTeX)

  val codeLike = Seq(Code)


  val urlAttributed = Seq(
    Link, Image
  )

  val coded = Seq(Code, LaTeX)

  val all: Seq[Delimitation] = coded ++ formatLike ++ urlAttributed

  val breakOthersOrdered: Seq[Delimitation] = formatLike ++ linkLike
  val noBreakeeOrdered: Seq[Delimitation] = linkLike
  //** from inner to outer
  // also inner splits
  val breakOthersOrderedUnicode: Seq[Unicode] = breakOthersOrdered.map(a => a.startUnicode)
  val noBreakeeOrderedUnicode: Seq[Unicode] = noBreakeeOrdered.map(a => a.startUnicode)

  val starts: Seq[SpecialChar] = all.map(_.start)
  val ends: Seq[SpecialChar] = all.map(_.end)

  val startsEnds: Seq[SpecialChar] = starts ++ ends

  case class Delimitation(start: SpecialChar, end: SpecialChar, attributes: Seq[SpecialChar] = Seq.empty, isAtomic: Boolean = false) {

    def wrap(a: Unicode = Unicode.empty): Unicode = Unicode(start).join(a).join(Unicode(attributes :+ end))


    private[model] def startUnicode = Unicode(start)
    private[model] def endUnicode = Unicode(end)

    def wrapSizeOffset = 2 + attributes.size
  }

}

