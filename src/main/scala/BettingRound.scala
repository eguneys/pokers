package poker

sealed trait BettingRound {

  val forsyth: Char

  def next: BettingRound

}

case object Preflop extends BettingRound {
  val forsyth = 'P'

  val next = Flop
}
case object Flop extends BettingRound {
  val forsyth = 'F'
  val next = Turn
}
case object Turn extends BettingRound {
  val forsyth = 'T'
  val next = River
}
case object River extends BettingRound {
  val forsyth = 'R'
  val next = River
}

object BettingRound {

  val all = List(Preflop, Flop, Turn, River)

  val allByForsyth = all.map { round => round.forsyth -> round } toMap

  def forsyth(c: Char): Option[BettingRound] = allByForsyth get c

}
