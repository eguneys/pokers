package poker

case class MiddleCards(
  flop: Option[List[Card]] = None,
  turn: Option[Card] = None,
  river: Option[Card] = None)
