package poker

trait HandValue {
  val rank: Int

  val high: Rank
  val sorted: List[Card]

  def kickers = sorted.foldLeft(0) { (acc, card) =>
    acc + card.rank.value
  }

  def magic = (rank * 9999) + kickers
}

case class HighCard(high: Rank, sorted: List[Card]) extends HandValue {
  val rank = 1
}
case class OnePair(high: Rank, sorted: List[Card]) extends HandValue {
  val rank = 2
}
case class TwoPair(high: Rank, low: Rank, sorted: List[Card]) extends HandValue {
  val rank = 3
}

case class ThreeOfAKind(high: Rank, sorted: List[Card]) extends HandValue {
  val rank = 4
}

case class Straight(high: Rank, sorted: List[Card]) extends HandValue {
  val rank = 5
}

case class Flush(high: Rank, sorted: List[Card]) extends HandValue {
  val rank = 6
}

case class FullHouse(high: Rank, low: Rank, sorted: List[Card]) extends HandValue {
  val rank = 7
}

case class FourOfAKind(high: Rank, sorted: List[Card]) extends HandValue {
  val rank = 8
}

case class StraightFlush(high: Rank, sorted: List[Card]) extends HandValue {
  val rank = 9
}
