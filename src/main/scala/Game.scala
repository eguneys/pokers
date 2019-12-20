package poker

case class Game(
  situation: Situation) {

  def apply(playerAct: PlayerAct): Valid[(Game, Move)] =
    situation.move(playerAct) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    val newSituation = move.situationAfter

    copy(
      situation = newSituation)
  }

  def dealer = situation.dealer

}

object Game {

  def apply(dealer: Dealer): Game = Game(dealer, HandDealer.shuffled(dealer.stacks.length))

  def apply(dealer: Dealer, handDealer: HandDealer): Game = new Game(Situation(dealer, handDealer))

}
