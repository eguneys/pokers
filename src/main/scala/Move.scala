package poker

case class Move(
  playerAct: PlayerAct,
  situationBefore: Situation,
  after: Dealer) {

  def dealerAct: DealerAct = {
    val iSituation = Situation(after, situationBefore.handDealer)

    val toAct = situationAfter.dealer.turnToAct
    val playerDiff = situationAfter.dealer.diff(situationBefore.dealer.turnToAct)

    if (iSituation.oneWin) {
      val winners = situationAfter.dealer.oneWinner

      OneWin(winners)
    } else if (iSituation.showdown) {
      val middle = iSituation.middle
      val hands = iSituation.showdownHands
      val handValueMagics = iSituation.handValueMagics
      val winners = situationAfter.dealer.showdownWinner(handValueMagics)
      Showdown(middle, hands, winners)
    } else if (iSituation.nextRound) {
      val middle = situationAfter.middle
      val runningPot = situationAfter.dealer.runningPot
      val sidePots = situationAfter.dealer.sidePots
      NextRound(toAct, playerDiff, middle, runningPot, sidePots)
    } else {
      NextTurn(toAct, playerDiff)
    }

  }

  def before = situationBefore.dealer

  def situationAfter = Situation(finalizeAfter, situationBefore.handDealer)

  def finalizeAfter: Dealer = {
    val iSituation = Situation(after, situationBefore.handDealer)

    if (iSituation.nextTurn) {
      after.nextTurn
    } else if (iSituation.nextRound) {
      after.nextRound
    } else if (iSituation.oneWin) {
      after.oneWin
    } else if (iSituation.showdown) {
      after.oneWin
    } else {
      after
    }
  }

}
