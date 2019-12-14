package poker

case class Actor(situation: Situation) {

  lazy val validMoves: List[Move] = Nil

  def validRaise(raise: Raise): Option[Move] = None

}
