package poker

sealed trait BettingRound {

  val forsyth: Char

}

case object Preflop extends BettingRound {
  val forsyth = 'P'
}
case object Flop extends BettingRound {
  val forsyth = 'F'
}
case object Turn extends BettingRound {
  val forsyth = 'T'
}
case object River extends BettingRound {
  val forsyth = 'R'
}

object BettingRound {

  val all = List(Preflop, Flop, Turn, River)

  val allByForsyth = all.map { round => round.forsyth -> round } toMap

  def forsyth(c: Char): Option[BettingRound] = allByForsyth get c

}
