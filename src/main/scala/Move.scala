package poker

case class Move(
  playerAct: PlayerAct,
  dealerAct: DealerAct,
  playerDiff: PlayerDiff,
  situationBefore: Situation,
  after: Dealer) {

  def before = situationBefore.dealer

  def situationAfter = Situation(finalizeAfter)

  def finalizeAfter: Dealer = after

}
