package poker

import scalaz.{ Validation => V }

class PlayTest extends PokerTest {

  "playing a game" should {

    "heads up" in {

      val dealer = """
10 P 0 1 10!0 0 1
I 90 10 .
I 95 5 .
"""

      val game = Game(dealer)

      "preflop SB acts first" in {
        game.playMoves(Call, RegularRaise(10)) must beGame("""
10 P 0 1 10!0 0 1
I 80 20 RR10
I 90 10 CA
""")
      }

      "preflop ends after wagers equalized" in {
        game.playMoves(Call, RegularRaise(10), Call) must beGame("""
10 F 0 1 10!40 0 1
I 80 0 .
I 80 0 .
""")
      }

    }

    "have possible raises" in {

      "pot raise" in {
        val dealer = """
10 P 0 1 10!0 0 1
I 90 10 .
I 95 5 .
"""

        val game = Game(dealer)

        V.success(game) must bePoss(Call, Fold, AllInNone, PotRaise(20), HalfPotRaise(10))
      }

      "third pot raise" in {
        val game = Game("""
10 P 0 0 10!0 0 1
I 90 10 .
I 95 30 RR20
""")

        V.success(game) must bePoss(Call, Fold, AllInNone, PotRaise(60), HalfPotRaise(30), ThirdPotRaise(20))

      }

    }

  }
}
