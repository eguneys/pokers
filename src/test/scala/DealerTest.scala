package poker

import format.Visual
import format.Visual.addNewLines

class DealerTest extends PokerTest {

  "a dealer" should {

    val dealer = makeDealer(100, List(1000, 1000, 800))

    "have involved players" in {
      dealer.stacks.map(_.role) must_== List(Involved, Involved, Involved)
    }

    "have blinds, turnToAct etc." in {
      dealer.button must_== 0
      dealer.SB must_== 1
      dealer.BB must_== 2
      dealer.turnToAct must_== 0

      dealer.lastFullRaise must_== 100
    }

    "have player acts" in {

      "not allow raise less than last full raise" in {
        dealer.raise(0) must beNone
        dealer.raise(99) must beNone
      }

      "not allow raise more than stack" in {
        dealer.raise(900) must beNone
      }

      "allow raise at least last full raise" in {
        dealer.raise(100) must beDealer("""
100 P 0 0 100!
I 800 200 RR100
I 950 50 .
I 700 100 .
""")
      }

      "not allow call more than stack" in {

        val dealer= Visual << """
100 P 0 2 100!
I 800 800 RR800
I 950 50 .
I 700 100 .
"""

        dealer.call() must beNone
      }

      "allow call" in {

        val dealer= Visual << """
100 P 0 2 100!
I 800 700 RR700
I 950 50 .
I 700 100 .
"""

        dealer.call() must beSome.like {
          case d => addNewLines(d.visual) must_== """
100 P 0 2 100!
I 800 700 RR700
I 950 50 .
I 100 700 CA
"""
        }
      }

      "not allow check when behind" in {
        val dealer= Visual << """
100 P 0 2 100!
I 800 700 RR700
I 950 50 .
I 700 100 .
"""

        dealer.check must beNone
      }

      "allow check when equal" in {

        val dealer= Visual << """
100 P 0 2 100!
I 800 700 RR700
I 950 50 .
I 700 700 .
"""

        dealer.check must beSome.like {
          case d => addNewLines(d.visual) must_== """
100 P 0 2 100!
I 800 700 RR700
I 950 50 .
I 700 700 CH
"""
        }
      }

      "allow fold" in {
        dealer.fold() must beDealer("""
100 P 0 0 100!
F 1000 0 FO
I 950 50 .
I 700 100 .
""")
      }

      "allow all in" in {

        "allow all in full" in {
          dealer.allin() must beDealer("""
100 P 0 0 900!
N 0 1000 AF
I 950 50 .
I 700 100 .
""")
        }

        "allow all in half" in {
          val dealer = Visual << """
100 P 0 1 100!
I 150 200 RR100
I 200 50 .
I 700 100 .
"""

          dealer.allin() must beDealer("""
100 P 0 1 100!
I 150 200 RR100
N 0 250 AH
I 700 100 .
""")
        }


        "allow all in call" in {
          val dealer = Visual << """
100 P 0 1 100!
I 150 200 RR100
I 100 50 .
I 700 100 .
"""

          dealer.allin() must beDealer("""
100 P 0 1 100!
I 150 200 RR100
N 0 150 AC
I 700 100 .
""")
        }

      }

    }


    "raise and all in affects last full raise" in {

      "full raise" in {

        dealer.raise(250) must beDealer("""
100 P 0 0 250!
I 650 350 RR250
I 950 50 .
I 700 100 .
""")
      }
    }

    "allow raise when facing a full raise" in {

          val dealer = Visual << """
100 F 0 0 100!
I 250 100 RR100
N 0 125 AA
I 200 125 CA
N 0 200 AA
I 200 200 CA
"""

      dealer.raise(100) must beDealer("""
100 F 0 0 100!
I 50 300 RR100
N 0 125 AA
I 200 125 CA
N 0 200 AA
I 200 200 CA
""")

    }

    "allow raise when not facing a full raise but hasn't acted" in {
          val dealer = Visual << """
4000 P 2 4 4000!
I 1000 4000 CA
F 1000 0 FO
N 0 7500 AH
F 1000 2000 FO
I 16000 4000 .
"""

      dealer.raise(4000) must beDealer("""
4000 P 2 4 4000!
I 1000 4000 CA
F 1000 0 FO
N 0 7500 AH
F 1000 2000 FO
I 8500 11500 RR4000
""")
    }

    "dont allow raise when not facing a full raise" in {

          val dealer = Visual << """
100 F 0 2 100!
I 50 200 CA
N 0 125 AA
I 200 125 CA
N 0 200 AA
I 200 200 CA
"""

      dealer.raise(100) must beNone

    }


    "have next turn" in {
      val dealer = Visual << """
100 F 0 0 100!
I 50 200 CA
N 0 125 AA
I 200 125 CA
N 0 200 AA
I 200 200 CA
"""

      Some(dealer.nextTurn) must beDealer("""
100 F 0 2 100!
I 50 200 CA
N 0 125 AA
I 200 125 CA
N 0 200 AA
I 200 200 CA
""")
    }


    "have next round" in {
      val dealer = Visual << """
100 F 0 0 100!
I 50 200 CA
N 0 125 AA
I 200 125 CA
N 0 200 AA
I 200 200 CA
"""

      Some(dealer.nextRound) must beDealer("""
100 T 0 2 100!
I 50 200 CA
O 0 125 AA
I 200 125 CA
O 0 200 AA
I 200 200 CA
""")
    }

  }

}
