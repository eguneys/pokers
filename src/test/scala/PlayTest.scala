package poker

class PlayTest extends PokerTest {

  "playing a game" should {

    "heads up" in {

      val dealer = """
10 P 0 1 0 10!
I 90 10 .
I 95 5 .
"""

      val game = Game(dealer)

      "preflop SB acts first" in {
        game.playMoves(Call,
          RegularRaise(10)) must beGame("""
10 P 0 1 0 10!
I 80 20 RR
I 90 10 CA
""")
      }

    }

  }
}
