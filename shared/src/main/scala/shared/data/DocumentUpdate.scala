package shared.data



case class DocumentUpdate(
  winners: Seq[Transaction],
  acceptedLosersCount: Int
)
