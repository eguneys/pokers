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

}
