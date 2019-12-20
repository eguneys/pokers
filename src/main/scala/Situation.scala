package poker

case class Situation(dealer: Dealer,
  handDealer: HandDealer) {

  lazy val actor = Actor(this)

  lazy val moves: List[Move] = actor.validMoves

  def raiseMove(raise: Raise): Option[Move] = actor.validRaise(raise)

  lazy val possibleActs: List[PlayerAct] = moves.map(_.playerAct)

  lazy val round: BettingRound = dealer.round

  lazy val middle: MiddleCards = handDealer.middle(round)

  lazy val showdownHands: List[Option[Hand]] = handDealer.showdownHands(dealer)

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

  def move(act: PlayerAct): Valid[Move] = {
    def findMove(act: PlayerAct) = act match {
      case r:Raise => raiseMove(r)
      case act => moves find(_.playerAct == act)
    }
    for {
      m <- findMove(act) toValid "Not a valid move"
    } yield m
  }
}
