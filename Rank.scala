package poker

trait Rank {
  val value: Int
  val forsyth: Char

  def of(suit: Suit) = Card(this, suit)
}

case class NormalRank(value: Int) extends Rank {
  val forsyth = value match {
    case 13 => 'K'
    case 12 => 'Q'
    case 11 => 'J'
    case 10 => 'T'
    case a => (a+'0').toChar
  }
}

case object AceHighRank extends Rank {
  val value = 14
  val forsyth = 'A'
}

case object AceLowRank extends Rank {
  val value = 1
  val forsyth = 'a'
}

object Rank {

  val rankCache = new Array[Rank](14)

  private[this] def createRank(r: Int): Rank = {
    val rank = r match {
      case 14 => AceHighRank
      case 1 => AceLowRank
      case r => NormalRank(r)
    }
    rankCache(r - 1) = rank
    rank
  }

  val AceLow = createRank(1)
  val Ace = createRank(14)
  val Two = createRank(2)
  val Three = createRank(3)
  val Four = createRank(4)
  val Five = createRank(5)
  val Six = createRank(6)
  val Seven = createRank(7)
  val Eight = createRank(8)
  val Nine = createRank(9)
  val Ten = createRank(10)
  val Jack = createRank(11)
  val Queen = createRank(12)
  val King = createRank(13)

  val all = rankCache.toList

  val allSorted = all.sortWith(_.value>_.value)

  val allByValue = all.map { rank => rank.value -> rank } toMap
  val allByForsyth = all.map { rank => rank.forsyth -> rank } toMap

  def forsyth(c: Char): Option[Rank] = allByForsyth get c

  implicit def fromInt(value: Int): Option[Rank] = allByValue get value

}
