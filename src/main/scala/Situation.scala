package poker

case class Situation(dealer: Dealer,
  handDealer: HandDealer) {

  def toAct: StackIndex = dealer.turnToAct

  lazy val actor = Actor(this)

  lazy val moves: List[Move] = actor.validMoves

  def raiseMove(raise: Raise): Option[Move] = actor.validRaise(raise)

  lazy val possibleActs: List[PlayerAct] = moves.map(_.playerAct)

  lazy val round: BettingRound = dealer.round

  lazy val middle: MiddleCards = handDealer.middle(round)

  lazy val handValueMagics: List[HandValueMagic] = handDealer.handValues(dealer)

  lazy val showdownHands: List[Option[Hand]] = handDealer.showdownHands(dealer)

  def nextTurn: Boolean = !nextRound && !end
  def nextRound: Boolean = roundEnd && round != River && !end

  def roundEnd: Boolean = dealer.allActed && dealer.wagersEqualized && dealer.involvedsHigherThanNewAllIns

  def showdown: Boolean = if (roundEnd) {
    (round == River && !oneWin) ||
    (dealer.oneInvolved && dealer.allInsExists)
  } else {
    !oneWin && dealer.noneInvolved
  }

  def oneWin: Boolean = (dealer.oneInvolved && !dealer.allInsExists) ||
  (dealer.noneInvolved && dealer.oneAllIn)
  
  def end: Boolean = showdown || oneWin

  lazy val status: Option[Status] =
    if (oneWin) Some(Status.OneWin)
    else if (showdown) Some(Status.Showdown)
    else None

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
