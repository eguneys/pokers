package poker

case class Situation(dealer: Dealer) {

  lazy val actor = Actor(this)

  lazy val moves: List[Move] = actor.validMoves

  def raiseMove(raise: Raise): Option[Move] = actor.validRaise(raise)

  lazy val possibleActs: List[PlayerAct] = moves.map(_.playerAct)

  lazy val round: BettingRound = dealer.round

  def nextTurn: Boolean = !nextRound && !end
  def nextRound: Boolean = roundEnd && round != River && !end

  def roundEnd: Boolean = dealer.allActed && dealer.wagersEqualized

  def showdown: Boolean = if (roundEnd) {
    (round == River && !oneWin) ||
    (dealer.oneInvolved && dealer.allInsExists)
  } else {
    dealer.noneInvolved
  }

  def oneWin: Boolean = dealer.oneInvolved && !dealer.allInsExists
  
  def end: Boolean = showdown || oneWin

  def move(act: PlayerAct): Valid[Move] = ???
}
