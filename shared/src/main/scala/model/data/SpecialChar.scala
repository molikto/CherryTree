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

  private object DelimitationType {

    val Atomic = 1
    val NonAtomic = 3
    val Empty = 2
    val Splittable = 4
    val NonSplittable = 5
    // all
    // 0coded                   non-coded
    // 1atomic 3non-atomic       4splittable 5non-splittable
    // 2empty
  }

  def apply(id: Int): SpecialChar = createSpecialChar(id)

  val attributes = Seq(UrlAttribute, TitleAttribute)

  val Emphasis =
    Delimitation("emphasis", EmphasisStart, EmphasisEnd, DelimitationType.Splittable)
  val Strong =
    Delimitation("strong", StrongStart, StrongEnd, DelimitationType.Splittable)
  val StrikeThrough =
    Delimitation("strike through", StrikeThroughStart, StrikeThroughEnd, DelimitationType.Splittable)
  val Link =
    Delimitation("link", LinkStart, LinkEnd, DelimitationType.NonSplittable, attributes)
  val Image =
    Delimitation("image", ImageStart, ImageEnd, DelimitationType.Empty, attributes)
  val Code =
    Delimitation("code", CodeStart, CodeEnd, DelimitationType.NonAtomic)
  val LaTeX =
    Delimitation("LaTeX", LaTeXStart, LaTeXEnd, DelimitationType.Atomic)

  val all: Seq[Delimitation] = Seq(Emphasis, Strong, StrikeThrough, Code, Link, LaTeX, Image)

  val nonCodedSplittable: Seq[Delimitation] = all.filter(_.ty == DelimitationType.Splittable)
  val nonCodedNonSplittable: Seq[Delimitation] = all.filter(_.ty == DelimitationType.NonSplittable)
  val atomicNonEmpty: Seq[Delimitation] =  all.filter(_.ty == DelimitationType.Atomic)
  val codedNonAtomic: Seq[Delimitation] =  all.filter(_.ty == DelimitationType.NonAtomic)
  val emptyContent: Seq[Delimitation] = all.filter(_.ty == DelimitationType.Empty)
  val codedNonEmpty: Seq[Delimitation] = all.filter(_.codedNonEmpty)
  val nonCoded = nonCodedSplittable ++ nonCodedNonSplittable


  val urlAttributed: Seq[Delimitation] = all.filter(_.attributes.contains(UrlAttribute))

  //** from inner to outer
  // also inner splits
  val breakOthersOrderedUnicode: Seq[Unicode] = nonCoded.map(a => a.startUnicode)
  val noBreakeeOrderedUnicode: Seq[Unicode] = nonCodedNonSplittable.map(a => a.startUnicode)

  val starts: Seq[SpecialChar] = all.map(_.start)
  val ends: Seq[SpecialChar] = all.map(_.end)

  val startsEnds: Seq[SpecialChar] = starts ++ ends

  case class Delimitation(name: String,
    start: SpecialChar,
    end: SpecialChar,
    ty: Int,
    attributes: Seq[SpecialChar] = Seq.empty
  ) {
    def wrap(a: Unicode = Unicode.empty): Unicode = Unicode(start) + a + Unicode(attributes :+ end)

    def atomic: Boolean = ty == DelimitationType.Atomic || ty == DelimitationType.Empty
    def codedNonEmpty: Boolean =  ty == DelimitationType.Atomic || ty == DelimitationType.NonAtomic

    private[model] def startUnicode = Unicode(start)
    private[model] def endUnicode = Unicode(end)

    def wrapSizeOffset = 2 + attributes.size
  }

}

