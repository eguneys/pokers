package poker

sealed trait Side {
  val index: Int
}

trait FiveSide extends Side
trait NineSide extends Side

object Side {

  type Map[A] = scala.collection.Map[Side, A]

  case object ZeroI extends FiveSide with NineSide {
    val index = 0
  }
  case object OneI extends FiveSide with NineSide {
    val index = 1
  }
  case object TwoI extends FiveSide with NineSide {
    val index = 2
  }
  case object ThreeI extends FiveSide with NineSide {
    val index = 3
  }
  case object FourI extends FiveSide with NineSide {
    val index = 4
  }
  case object FiveI extends NineSide {
    val index = 5
  }
  case object SixI extends NineSide {
    val index = 6
  }
  case object SevenI extends NineSide {
    val index = 7
  }
  case object EightI extends NineSide {
    val index = 8
  }

  def all = List(
    ZeroI,
    OneI,
    TwoI,
    ThreeI,
    FourI,
    FiveI,
    SixI,
    SevenI,
    EightI
  )

  def allByForsyth = all map { s => s.index.toString -> s } toMap

  def apply(str: String): Option[Side] = allByForsyth get str

  def orDefault(str: String): Side = Side(str) getOrElse ZeroI
}

abstract class NbSeats {
  val nb: Int
  val allIndexes: List[Side]
  def valid(side: Side): Boolean = allIndexes contains side
}

case object NbSeats {

  import Side._

  case object Five extends NbSeats {
    val nb = 5
    val allIndexes = List(
      ZeroI,
      OneI,
      TwoI,
      ThreeI,
      FourI)
  }
  case object Nine extends NbSeats {
    val nb = 9
    val allIndexes = List(
      ZeroI,
      OneI,
      TwoI,
      ThreeI,
      FourI,
      FiveI,
      SixI,
      SevenI,
      EightI)
  }

}
