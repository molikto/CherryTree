package model.data


trait SpecialCharTrait extends Enumeration {
  type SpecialChar = Value

  private[data] def createSpecialChar(id: Int) = apply(id)


  val EmphasisStart, EmphasisEnd,
    StrongStart, StrongEnd,
    StrikeThroughStart, StrikeThroughEnd,
    LinkStart, LinkEnd,
    ImageStart, ImageEnd,
    CodeStart, CodeEnd,
    LaTeXStart, LaTeXEnd,
    UrlAttribute, TitleAttribute,
    HTMLStart, HTMLEnd,
    HashTagStart, HashTagEnd,
    HashDefStart, HashDefEnd,
    SupStart, SupEnd,
    SubStart, SubEnd,
    UnderlineStart, UnderlineEnd,
    SpanClassStart, SpanClassEnd,
    ClassAttribute
  :
    SpecialChar = Value
}

object SpecialChar {

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

  private val urlAndTitleAttributes = Seq(UrlAttribute, TitleAttribute)

  val Emphasis =
    Delimitation("emphasis", EmphasisStart, EmphasisEnd, DelimitationType.FormattedSplittable)
  val Strong =
    Delimitation("strong", StrongStart, StrongEnd, DelimitationType.FormattedSplittable)
  val StrikeThrough =
    Delimitation("strike through", StrikeThroughStart, StrikeThroughEnd, DelimitationType.FormattedSplittable)
  val Link =
    Delimitation("link", LinkStart, LinkEnd, DelimitationType.FormattedNonSplittable, urlAndTitleAttributes)
  val HashTag =
    Delimitation("hashtag", HashTagStart, HashTagEnd, DelimitationType.FormattedNonSplittable)
  val SpanClass =
    Delimitation("span with class", SpanClassStart, SpanClassEnd, DelimitationType.FormattedNonSplittable, attributes = Seq(ClassAttribute))
  val Sup =
    Delimitation("sup", SupStart, SupEnd, DelimitationType.FormattedNonSplittable)
  val Sub =
    Delimitation("sub", SubStart, SubEnd, DelimitationType.FormattedNonSplittable)
  val Underline =
    Delimitation("underline", UnderlineStart, UnderlineEnd, DelimitationType.FormattedSplittable)
  val HashDef =
    Delimitation("hashdef", HashDefStart, HashDefEnd, DelimitationType.FormattedNonSplittable)
  val Image =
    Delimitation("image", ImageStart, ImageEnd, DelimitationType.Empty, urlAndTitleAttributes)
  val Code =
    Delimitation("code", CodeStart, CodeEnd, DelimitationType.CodedNonAtomic)
  val LaTeX =
    Delimitation("LaTeX", LaTeXStart, LaTeXEnd, DelimitationType.CodedAtomic, codeType = Embedded.LaTeX)

  val HTML =
    Delimitation("html", HTMLStart, HTMLEnd, DelimitationType.CodedAtomic, codeType = Embedded.HTML)

  val all: Seq[Delimitation] = Seq(Emphasis, Strong, StrikeThrough, Code, Link, HashTag, HashDef, LaTeX, Image, HTML, Sub, Sup, Underline, SpanClass)

  val formattedSplittable: Seq[Delimitation] = all.filter(_.ty == DelimitationType.FormattedSplittable)
  val formattedNonSplittable: Seq[Delimitation] = all.filter(_.ty == DelimitationType.FormattedNonSplittable)
  val formatted: Seq[Delimitation] = formattedSplittable ++ formattedNonSplittable

  val codedAtomic: Seq[Delimitation] =  all.filter(_.ty == DelimitationType.CodedAtomic)
  val codedNonAtomic: Seq[Delimitation] =  all.filter(_.ty == DelimitationType.CodedNonAtomic)
  val coded: Seq[Delimitation] = all.filter(_.coded)


  val emptyContent: Seq[Delimitation] = all.filter(_.ty == DelimitationType.Empty)

  val nonAtomic: Seq[Delimitation] = all.filter(a => a.ty != DelimitationType.Empty && a.ty != DelimitationType.CodedAtomic)

  val urlAttributed: Seq[Delimitation] = all.filter(_.attributes.contains(UrlAttribute))

  //** from inner to outer
  // also inner splits
  val breakOthersOrderedUnicode: Seq[SpecialChar] = formatted.map(a => a.start)
  val noBreakeeOrderedUnicode: Seq[SpecialChar] = formattedNonSplittable.map(a => a.start)

  val starts: Seq[SpecialChar] = all.map(_.start)
  val ends: Seq[SpecialChar] = all.map(_.end)

  val startsEnds: Seq[SpecialChar] = starts ++ ends

  case class Delimitation(name: String,
    start: SpecialChar,
    end: SpecialChar,
    ty: Int,
    attributes: Seq[SpecialChar] = Seq.empty,
    codeType: CodeType = null
  ) {
    private[model] def wrap(a: EncodedSeq = EncodedSeq.empty): EncodedSeq = EncodedSeq(start) + a + EncodedSeq(attributes :+ end)

    def atomic: Boolean = ty == DelimitationType.CodedAtomic || ty == DelimitationType.Empty
    def coded: Boolean =  ty == DelimitationType.CodedAtomic || ty == DelimitationType.CodedNonAtomic
    def codedNonAtomic: Boolean = ty == DelimitationType.CodedNonAtomic
    def codedAtomic: Boolean = ty == DelimitationType.CodedAtomic

    def newSkipSize: Int = attributes.size
    def newDeliEndSize: Int = newSkipSize + 1
    def newDeliStartSize = 1
    def wrapSizeOffset: Int = newDeliEndSize + 1

    def emptySize: Int = 2 + attributes.size
  }

}

