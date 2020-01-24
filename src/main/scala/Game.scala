package poker

import format.{ Uci }

case class Game(
  situation: Situation,
  clock: Option[Clock] = None) {

  def apply(playerAct: PlayerAct): Valid[(Game, Move)] =
    situation.move(playerAct) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    val newSituation = move.situationAfter

    copy(
      situation = newSituation,
      clock = applyClock)
  }

  def apply(uci: Uci.Move): Valid[(Game, Move)] = apply(uci.playerAct)

  private def applyClock = clock.map { c =>
    val newC = c.step
    if (!newC.isRunning) newC.start else newC
  }

  def player = situation.toAct

  def dealer = situation.dealer

  def handDealer = situation.handDealer

}

object Game {

  def apply(button: StackIndex, iStacks: List[Chips]): Game =
    Game(Dealer.empty(button, iStacks))

  def apply(dealer: Dealer): Game = Game(dealer, HandDealer.shuffled(dealer.stacks.length))

  def apply(dealer: Dealer, handDealer: HandDealer): Game = new Game(Situation(dealer, handDealer))

}
