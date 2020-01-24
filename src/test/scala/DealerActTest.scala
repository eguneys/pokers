package poker

class DealerActTest extends PokerTest {

  "playing a game" should {

    val dealer = Dealer.empty(0, List(100, 100))

    val game = Game(dealer)

    "collect pots on one win" in {

      val g = game.playMoves(AllInNone).getOrElse(null)

      g(Fold) must haveDealerAct {
        case OneWin(winners) =>
          winners must_== Winners(List(PotDistribution(102, List(1))),
            List(98, 102))
      }
      
    }

    "collect pots on showdown" in {

      val g = game.playMoves(AllInNone).getOrElse(null)

      g(AllInNone) must haveDealerAct {
        case Showdown(_, _, winners) =>
          println(winners)
          3 must_== 3
      }
      
    }

  }
}
