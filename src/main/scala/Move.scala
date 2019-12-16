package poker

case class Move(
  playerAct: PlayerAct,
  situationBefore: Situation,
  after: Dealer) {

  def dealerAct: DealerAct = ???
  def playerDiff: PlayerDiff = ???

  def before = situationBefore.dealer

  def situationAfter = Situation(finalizeAfter)

  def finalizeAfter: Dealer = after

}
