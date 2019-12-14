package poker

sealed trait DealerAct {
}

case class NextTurn(toAct: StackIndex) extends DealerAct {
}

case class NextRound(toAct: StackIndex, middle: MiddleCards, runningPot: Pot, sidePots: List[Pot]) extends DealerAct {
}

case class OneWin(winners: Winners) extends DealerAct {

}

case class Showdown(middle: MiddleCards, hands: List[Option[Hand]], winners: Winners) extends DealerAct {

}
