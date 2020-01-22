package poker

class SituationTest extends PokerTest {

  "a game" should {

    "detect next turn" in {
      ("""
10 P 0 0 10!0 0 1 2
I 100 0 .
I 100 5 .
I 100 10 .
""").nextTurn must beTrue

    }

    "detect next round" in {
      ("""
10 P 0 0 10!0 0 1 2
I 100 10 CA
I 100 10 CA
I 100 10 CH
""").nextRound must beTrue

      ("""
10 P 0 0 10!0 0 1 2
I 100 10 CA
F 100 5 FO
I 100 10 CH
""").nextRound must beTrue


      ("""
10 P 0 0 10!0 0 1 2
I 100 10 CA
N 0 5 AA
I 100 10 CH
""").nextRound must beTrue

    }

    "detect showdown" in {
      ("""
10 R 0 0 10!0 0 1 2
I 100 10 CA
I 100 10 CA
I 100 10 CH
""").showdown must beTrue

      ("""
10 P 0 0 10!0 0 1 2
N 0 90 AA
N 0 10 AA
I 0 100 CA
""").showdown must beTrue

    }

    "detect onewin" in {
      ("""
10 R 0 0 10!0 0 1 2
F 100 10 FO
F 100 10 FO
I 100 10 .
""").oneWin must beTrue
    }


    "detect next turn after all in" in {
      val dealer = Dealer.empty(10f, 0, List(100f, 100f))

      val game = Game(dealer)

      game.playMoves(Call, AllInNone) must beSuccess.like {
        case g =>
          g.situation.showdown must beFalse
          g.situation.nextTurn must beTrue
      }
    }
  }
}
