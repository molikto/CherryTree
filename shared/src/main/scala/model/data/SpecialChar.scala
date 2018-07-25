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
    Delimitation("emphasis", EmphasisStart, EmphasisEnd)
  val Strong =
    Delimitation("strong", StrongStart, StrongEnd)
  val StrikeThrough =
    Delimitation("strike through", StrikeThroughStart, StrikeThroughEnd)


  val attributes = Seq(UrlAttribute, TitleAttribute)

  val Link =
    Delimitation("link", LinkStart, LinkEnd, attributes)

  val Image =
    Delimitation("image", ImageStart, ImageEnd, attributes, isAtomic = true)


  val Code =
    Delimitation("code", CodeStart, CodeEnd)

  val LaTeX =
    Delimitation("LaTeX", LaTeXStart, LaTeXEnd, isAtomic = true)

  /**
    * things with content, split-able
    */
  val formatLike = Seq(
    Emphasis, Strong, StrikeThrough
  )

  /**
    * things with content, not split-able
    */
  val linkLike = Seq(Link)

  /**
    * coded, no content
    */
  val imageLike = Seq(Image)


  /**
    * coded, atomic
    */
  val latexLike = Seq(LaTeX)

  /**
    * coded, not atomic
    */
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

  case class Delimitation(name: String, start: SpecialChar, end: SpecialChar, attributes: Seq[SpecialChar] = Seq.empty, isAtomic: Boolean = false) {

    def wrap(a: Unicode = Unicode.empty): Unicode = Unicode(start) + a + Unicode(attributes :+ end)


    private[model] def startUnicode = Unicode(start)
    private[model] def endUnicode = Unicode(end)

    def wrapSizeOffset = 2 + attributes.size
  }

}

