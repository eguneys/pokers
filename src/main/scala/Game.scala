package poker

case class Game(
  situation: Situation) {

  def apply(playerAct: PlayerAct): Valid[(Game, Move)] =
    situation.move(playerAct) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    this
  }

  def dealer = situation.dealer

}

object Game {

  def apply(dealer: Dealer): Game = new Game(Situation(dealer))

}
