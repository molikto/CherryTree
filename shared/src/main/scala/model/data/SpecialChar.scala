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
  def special(a: Int): Boolean = {
    a >= SpecialCharStart && a <= SpecialCharStart + 0xFF
  }


  private object DelimitationType {

    val CodedAtomic = 1
    val CodedNonAtomic = 3
    val Empty = 2
    val FormattedSplittable = 4
    val FormattedNonSplittable = 5
    // all
    // 0coded                   non-coded                        empty
    // 1atomic 3non-atomic       4splittable 5non-splittable
  }

  def apply(id: Int): SpecialChar = createSpecialChar(id)

  val attributes = Seq(UrlAttribute, TitleAttribute)

  val Emphasis =
    Delimitation("emphasis", EmphasisStart, EmphasisEnd, DelimitationType.FormattedSplittable)
  val Strong =
    Delimitation("strong", StrongStart, StrongEnd, DelimitationType.FormattedSplittable)
  val StrikeThrough =
    Delimitation("strike through", StrikeThroughStart, StrikeThroughEnd, DelimitationType.FormattedSplittable)
  val Link =
    Delimitation("link", LinkStart, LinkEnd, DelimitationType.FormattedNonSplittable, attributes)
  val Image =
    Delimitation("image", ImageStart, ImageEnd, DelimitationType.Empty, attributes)
  val Code =
    Delimitation("code", CodeStart, CodeEnd, DelimitationType.CodedNonAtomic)
  val LaTeX =
    Delimitation("LaTeX", LaTeXStart, LaTeXEnd, DelimitationType.CodedAtomic)

  val all: Seq[Delimitation] = Seq(Emphasis, Strong, StrikeThrough, Code, Link, LaTeX, Image)

  val formattedSplittable: Seq[Delimitation] = all.filter(_.ty == DelimitationType.FormattedSplittable)
  val formattedNonSplittable: Seq[Delimitation] = all.filter(_.ty == DelimitationType.FormattedNonSplittable)
  val formatted: Seq[Delimitation] = formattedSplittable ++ formattedNonSplittable

  val codedAtomic: Seq[Delimitation] =  all.filter(_.ty == DelimitationType.CodedAtomic)
  val codedNonAtomic: Seq[Delimitation] =  all.filter(_.ty == DelimitationType.CodedNonAtomic)
  val coded: Seq[Delimitation] = all.filter(_.coded)

  val emptyContent: Seq[Delimitation] = all.filter(_.ty == DelimitationType.Empty)


  val urlAttributed: Seq[Delimitation] = all.filter(_.attributes.contains(UrlAttribute))

  //** from inner to outer
  // also inner splits
  val breakOthersOrderedUnicode: Seq[Unicode] = formatted.map(a => a.startUnicode)
  val noBreakeeOrderedUnicode: Seq[Unicode] = formattedNonSplittable.map(a => a.startUnicode)

  val starts: Seq[SpecialChar] = all.map(_.start)
  val ends: Seq[SpecialChar] = all.map(_.end)

  val startsEnds: Seq[SpecialChar] = starts ++ ends

  case class Delimitation(name: String,
    start: SpecialChar,
    end: SpecialChar,
    ty: Int,
    attributes: Seq[SpecialChar] = Seq.empty
  ) {
    def wrap(a: Unicode = Unicode.empty): Unicode = Unicode(start) + a + Unicode.specials(attributes :+ end)

    def atomic: Boolean = ty == DelimitationType.CodedAtomic || ty == DelimitationType.Empty
    def coded: Boolean =  ty == DelimitationType.CodedAtomic || ty == DelimitationType.CodedNonAtomic
    def codedNonAtomic = ty == DelimitationType.CodedNonAtomic

    private[model] def startUnicode = Unicode(start)
    private[model] def endUnicode = Unicode(end)

    def newSkipSize = attributes.size
    def wrapSizeOffset = 2 + newSkipSize
  }

}

