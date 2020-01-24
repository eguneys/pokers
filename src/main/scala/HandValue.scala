package poker

abstract class HandValue(
  val key: String,
  val name: String,
  val rank: Int) {

  val high: Rank

  val sorted: List[Card]

  def kickers = sorted.foldLeft(0) { (acc, card) =>
    acc + card.rank.value
  }

  def magic = (rank * 9999) + kickers

}

case class HighCard(high: Rank, sorted: List[Card]) extends HandValue(
  key = "highcard",
  name = "High Card",
  rank = 1) {

}
case class OnePair(high: Rank, sorted: List[Card]) extends HandValue(
  key = "onepair",
  name = "One Pair",
  rank = 2)

case class TwoPair(high: Rank, low: Rank, sorted: List[Card]) extends HandValue(
  key = "twopair",
  name = "Two Pair",
  rank = 3)

case class ThreeOfAKind(high: Rank, sorted: List[Card]) extends HandValue(
  key = "threeofakind",
  name = "Three Of A Kind",
  rank = 4)

case class Straight(high: Rank, sorted: List[Card]) extends HandValue(
  key = "straight",
  name = "Straight",
  rank = 5)

case class Flush(high: Rank, sorted: List[Card]) extends HandValue(
  key = "flush",
  name = "Flush",
  rank = 6)

case class FullHouse(high: Rank, low: Rank, sorted: List[Card]) extends HandValue(
  key = "fullhouse",
  name = "Full House",
  rank = 7)

case class FourOfAKind(high: Rank, sorted: List[Card]) extends HandValue(
  key = "fourofakind",
  name = "Four Of A Kind",
  rank = 8)

case class StraightFlush(high: Rank, sorted: List[Card]) extends HandValue(
  key = "straightflush",
  name = "Straight Flush",
  rank = 9)
