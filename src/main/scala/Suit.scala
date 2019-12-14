package poker

sealed trait Suit {
  val forsyth: Char
}

case object Hearts extends Suit {
  val forsyth = 'h'
}
case object Clubs extends Suit {
  val forsyth = 'c'
}
case object Diamonds extends Suit {
  val forsyth = 'd'
}
case object Spades extends Suit {
  val forsyth = 's'
}

object Suit {

  val all = List(Hearts, Clubs, Diamonds, Spades)

  val allByForsyth = all map { r => r.forsyth -> r } toMap

  def forsyth(c: Char): Option[Suit] = allByForsyth get c

}
