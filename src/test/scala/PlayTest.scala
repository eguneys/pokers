package poker

import scalaz.{ Validation => V }

class PlayTest extends PokerTest {

  "playing a game" should {

    "bare bones" in {
      val dealer = Dealer.empty(0, List(100, 100))

      val game = Game(dealer)

      "should allow next round" in {
        game.playMoves(Call, Check) must beGame("""
F 0 1 2!4 0 1
I 98 0 .
I 98 0 .
""")
      }

      "should not allow call on first act on flop" in {
        game.playMoves(Call, Call, Call) must beFailure
      }

      "should allow check check on flop" in {
        game.playMoves(Call, Check, Check, Check) must beGame("""
T 0 1 2!4 0 1
I 98 0 .
I 98 0 .
""")
      }

      "should allow all in fold" in {
        game.playMoves(AllInNone, Fold) must beGame("""
P 0 0 98!102 1
F 98 0 .
O 0 0 .
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

    "all in" in {
      val dealer = Dealer.empty(0, List(100, 100))

      val game = Game(dealer)

      val fourWay = Game(Dealer.empty(0, List(100, 100, 100, 100)))

      "should allow next turn after all in" in {
        game.playMoves(Call, AllInNone) must beGame("""
P 0 1 98!0 0 1
N 0 100 AF
I 98 2 CA
""")
      }

      "should not allow check after all in" in {
        fourWay.playMoves(Call, Call, Call, Check, AllInNone, Check) must beFailure
      }

      "should not allow check after all in fold" in {
        fourWay.playMoves(Call, Call, Call, Check, Check, Check, Check, AllInNone, Fold, Check) must beFailure
      }

      "should not allow call for all in amount" in {
        fourWay.playMoves(Call, Call, Call, Check, 
          Check, Check, Check, AllInNone,
          Fold, Call) must beFailure
      }

    }


    "heads up" in {

      val dealer = """
P 0 1 10!0 0 1
I 90 10 .
I 95 5 .
"""

      val game = Game(dealer)

      "preflop SB acts first" in {
        game.playMoves(Call, RegularRaise(10)) must beGame("""
P 0 1 10!0 0 1
I 80 20 RR10
I 90 10 CA
""")
      }

      "preflop ends after wagers equalized" in {
        game.playMoves(Call, RegularRaise(10), Call) must beGame("""
F 0 1 10!40 0 1
I 80 0 .
I 80 0 .
""")
      }

    }

    "have possible raises" in {

      "pot raise" in {
        val dealer = """
P 0 1 10!0 0 1
I 90 10 .
I 95 5 .
"""

        val game = Game(dealer)

        V.success(game) must bePoss(Call, Fold, AllInNone, 
          RegularRaise(10),
          PotRaise(20), HalfPotRaise(10))
      }

      "third pot raise" in {
        val game = Game("""
P 0 0 10!0 0 1
I 90 10 .
I 95 30 RR20
""")

        V.success(game) must bePoss(Call, Fold, AllInNone, RegularRaise(10), PotRaise(60), HalfPotRaise(30), ThirdPotRaise(20))

      }

    }

  }
}
