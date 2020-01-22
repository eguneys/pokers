package poker

case class HandDealer(flop: List[Card],
  turn: Card,
  river: Card,
  hands: List[(Card, Card)]) {

  def middle = flop :+ turn :+ river

  def hand(i: StackIndex) = Hand(List(hands(i)._1, hands(i)._2) ++ middle)


  def middle(round: BettingRound): MiddleCards = round match {
    case Flop => MiddleCards(flop = Some(flop))
    case Turn => MiddleCards(turn = Some(turn))
    case River => MiddleCards(river = Some(river))
    case _ => MiddleCards()
  }

  def handValues(dealer: Dealer): List[HandValueMagic] = dealer.stacks.zipWithIndex.map { case (_, i) => hand(i).value.magic
  }.toList

  def showdownHands(dealer: Dealer): List[Option[Hand]] = dealer.stacks.zipWithIndex.map { case (stack, _) if stack is Folded => None
    case (_, i) => Some(hand(i))
  }.toList

}


object HandDealer {

  import Rank._

  def apply(deck: List[Card], nbPlayers: Int): HandDealer = {
    val hands = List.tabulate(nbPlayers)(i => {
      deck.drop(3 + 3 + i * 2) match {
        case f :: s :: tail => (f, s)
        case _ => (Ace of Diamonds, Ace of Diamonds)
      }
    })

    HandDealer(flop = deck.take(3),
      turn = deck.drop(3).head,
      river = deck.drop(3 + 1).head,
      hands)
  }


  def shuffled(nbPlayers: Int): HandDealer = apply(scala.util.Random.shuffle(deck), nbPlayers)

  val deck = List(
    Ace of Diamonds,
    Two of Diamonds,
    Three of Diamonds,
    Four of Diamonds,
    Five of Diamonds,
    Six of Diamonds,
    Seven of Diamonds,
    Eight of Diamonds,
    Nine of Diamonds,
    Ten of Diamonds,
    Jack of Diamonds,
    Queen of Diamonds,
    King of Diamonds,

    Ace of Hearts,
    Two of Hearts,
    Three of Hearts,
    Four of Hearts,
    Five of Hearts,
    Six of Hearts,
    Seven of Hearts,
    Eight of Hearts,
    Nine of Hearts,
    Ten of Hearts,
    Jack of Hearts,
    Queen of Hearts,
    King of Hearts,

    Ace of Spades,
    Two of Spades,
    Three of Spades,
    Four of Spades,
    Five of Spades,
    Six of Spades,
    Seven of Spades,
    Eight of Spades,
    Nine of Spades,
    Ten of Spades,
    Jack of Spades,
    Queen of Spades,
    King of Spades,

    Ace of Clubs,
    Two of Clubs,
    Three of Clubs,
    Four of Clubs,
    Five of Clubs,
    Six of Clubs,
    Seven of Clubs,
    Eight of Clubs,
    Nine of Clubs,
    Ten of Clubs,
    Jack of Clubs,
    Queen of Clubs,
    King of Clubs
  )

}
