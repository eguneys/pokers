package poker

case class Hand(cards: List[Card]) {
  def sorted: List[Card] = value.sorted

  def value: HandValue = HandValueSolver.solve(this)

  override def toString = cards mkString " "
}

object Hand {

  def sorter(a: Hand, b: Hand) = a.value.magic > b.value.magic

}
