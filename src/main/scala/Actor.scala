package poker

case class Actor(situation: Situation) {

  def dealer: Dealer = situation.dealer

  lazy val validMoves: List[Move] = 
    (PlayerAct.all flatMap {
      case Check => dealer.check map { move(Check, _) }
      case Call => dealer.call map { move(Call, _) }
      case Fold => dealer.fold map { move(Fold, _) }
      case AllInNone => dealer.allin map { move(AllInNone, _) }
      case _ => None
    }) ++ (dealer.validRaises flatMap { 
      case raise:Raise => dealer.raise(raise.to) map { move(raise, _) }
    })

  def validRaise(raise: Raise): Option[Move] = dealer.raise(raise.to) map { move(raise, _) }

  private def move(act: PlayerAct,
    after: Dealer) = {
    Move(
      playerAct = act,
      situationBefore = situation,
      after = after)
  }

}
