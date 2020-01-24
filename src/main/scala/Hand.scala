package poker

case class Hand(holes: (Card, Card), middle: List[Card]) {

  def cards: List[Card] = holes._1 :: holes._2 :: middle

  def sorted: List[Card] = value.sorted

  def value: HandValue = HandValueSolver.solve(this)

  def holeString: String = holes._1.visual + " " + holes._2.visual

  override def toString = cards mkString " "
}

object Hand {

  def sorter(a: Hand, b: Hand) = a.value.magic > b.value.magic

}
