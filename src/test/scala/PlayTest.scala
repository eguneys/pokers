package poker

import scalaz.{ Validation => V }

class PlayTest extends PokerTest {

  "playing a game" should {

    "bare bones" in {
      val dealer = Dealer.empty(10f, 0, List(100f, 100f))

      val game = Game(dealer)

      "should allow next round" in {
        game.playMoves(Call, Check) must beGame("""
10 F 0 1 10!20 0 1
I 90 0 .
I 90 0 .
""")
      }

      "should not allow call on first act on flop" in {
        game.playMoves(Call, Call, Call) must beFailure
      }

      "should allow check check on flop" in {
        game.playMoves(Call, Check, Check, Check) must beGame("""
10 T 0 1 10!20 0 1
I 90 0 .
I 90 0 .
""")
      }

      "should distribute after showdown" in {
        val game2 = game.playMoves(Call, Check,
          Check, Check,
          Check, Check,
          Check) getOrElse null

        game2(Check) must beSuccess.like {
          case (g, m) => {
            Some(m.dealerAct) must beSome.like {
              case Showdown(_, _, winners) =>
                3 must_== 3
            }
          }
        }
      }

    }


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
