package poker
package format

sealed trait Uci {

  def uci: String

  def playerAct: PlayerAct

  def apply(situation: Situation): Valid[Move]

}


object Uci {

  case class Move(
    playerAct: PlayerAct) extends Uci {

    def uci = playerAct.uci

    def apply(situation: Situation) = situation.move(playerAct)

  }

  object Move {

    def apply(move: String): Option[Move] = PlayerAct.forsyth(move) orElse PlayerAct.Raise.forsyth(move) map { Move(_) }


  }

}
