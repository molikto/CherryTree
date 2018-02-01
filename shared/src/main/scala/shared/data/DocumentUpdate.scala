package shared.data



final case class DocumentUpdate(
  winners: Seq[Transaction],
  acceptedLosersCount: Int
)
