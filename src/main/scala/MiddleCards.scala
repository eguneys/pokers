package poker

case class MiddleCards(
  flop: Option[List[Card]],
  turn: Option[Card],
  river: Option[Card])
