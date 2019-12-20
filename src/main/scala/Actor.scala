package poker

case class Actor(situation: Situation) {

  def dealer: Dealer = situation.dealer

  lazy val validMoves: List[Move] = 
    PlayerAct.all flatMap {
      case Check => dealer.check map { move(Check, _) }
      case Call => dealer.call map { move(Call, _) }
      case _ => None
    }

  def validRaise(raise: Raise): Option[Move] = dealer.raise(raise.to) map { move(raise, _) }

  private def move(act: PlayerAct,
    after: Dealer) = {
    Move(
      playerAct = act,
      situationBefore = situation,
      after = after)
  }

}
