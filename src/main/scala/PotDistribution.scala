package poker

import poker.format.Visual

case class PotDistribution(
  wager: Chips,
  involved: List[StackIndex]) {

  def asPot = Pot(wager, involved)

  def visual = Visual writePot asPot

}
