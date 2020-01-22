package poker

sealed trait PlayerAct {

  val uci: String

}

abstract class Raise extends PlayerAct {

  override val uci = "RR"

  val to: Chips

} // ~ RR TR HR PR

case class RegularRaise(to: Chips) extends Raise {
  override val uci = "RR" + to
}

case class ThirdPotRaise(to: Chips) extends Raise {
  override val uci = "TR" + to
}

case class HalfPotRaise(to: Chips) extends Raise {
  override val uci = "HR" + to
}

case class PotRaise(to: Chips) extends Raise {
  override  val uci = "PR" + to
}
  
case object Call extends PlayerAct {
  override  val uci = "CA"
} // ~ CA

case object Check extends PlayerAct {
  override  val uci = "CH"
} // ~ CH

case object Fold extends PlayerAct {
  override  val uci = "FO"
} // ~ FO
  
abstract class AllIn extends PlayerAct {
  override  val uci = "AA"
} // ~ AA AC AH AF

case object AllInNone extends AllIn {
  override  val uci = "AA"
}

case object AllInCall extends AllIn {
  override  val uci = "AC"
}

case object AllInHalfRaise extends AllIn {
  override  val uci = "AH"
}

case object AllInFullRaise extends AllIn {
  override  val uci = "AF"
}

case object PlayerAct {

  def forsyth(s: String): Option[PlayerAct] = allByForsyth get s

  val all = List(Call, Check, Fold, AllInNone, AllInCall, AllInHalfRaise, AllInFullRaise)

  val allByForsyth = all map { a => a.uci -> a } toMap

  object Raise {

    def forsyth(s: String): Option[Raise] = s.take(2) match {
      case "RR" => Some(RegularRaise(Chips(s.drop(2).toFloat)))
      case _ => None
    }

  }

}
