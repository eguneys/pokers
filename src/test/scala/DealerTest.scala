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

      dealer.lastFullRaise must_== Chips(100)
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
100 P 0 0 100!0 0 1 2
I 800 200 RR100
I 950 50 .
I 700 100 .
""")
      }

      "not allow call more than stack" in {

        val dealer= Visual << """
100 P 0 2 100!0 0 1 2
I 800 800 RR800
I 950 50 .
I 700 100 .
"""

        dealer.call() must beNone
      }

      "allow call" in {

        val dealer= Visual << """
100 P 0 2 100!0 0 1 2
I 800 700 RR700
I 950 50 .
I 700 100 .
"""

        dealer.call() must beSome.like {
          case d => addNewLines(d.visual) must_== """
100 P 0 2 100!0 0 1 2
I 800 700 RR700
I 950 50 .
I 100 700 CA
"""
        }
      }

      "not allow check when behind" in {
        val dealer= Visual << """
100 P 0 2 100!0 0 1 2
I 800 700 RR700
I 950 50 .
I 700 100 .
"""

        dealer.check must beNone
      }

      "allow check when equal" in {

        val dealer= Visual << """
100 P 0 2 100!0 0 1 2
I 800 700 RR700
I 950 50 .
I 700 700 .
"""

        dealer.check must beSome.like {
          case d => addNewLines(d.visual) must_== """
100 P 0 2 100!0 0 1 2
I 800 700 RR700
I 950 50 .
I 700 700 CH
"""
        }
      }

      "allow fold" in {
        dealer.fold() must beDealer("""
100 P 0 0 100!0 0 1 2
F 1000 0 FO
I 950 50 .
I 700 100 .
""")
      }

      "allow all in" in {

        "allow all in full" in {
          dealer.allin() must beDealer("""
100 P 0 0 900!0 0 1 2
N 0 1000 AF
I 950 50 .
I 700 100 .
""")
        }

        "allow all in half" in {
          val dealer = Visual << """
100 P 0 1 100!0 0 1 2
I 150 200 RR100
I 200 50 .
I 700 100 .
"""

          dealer.allin() must beDealer("""
100 P 0 1 100!0 0 1 2
I 150 200 RR100
N 0 250 AH
I 700 100 .
""")
        }


        "allow all in call" in {
          val dealer = Visual << """
100 P 0 1 100!0 0 1 2
I 150 200 RR100
I 100 50 .
I 700 100 .
"""

          dealer.allin() must beDealer("""
100 P 0 1 100!0 0 1 2
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
100 P 0 0 250!0 0 1 2
I 650 350 RR250
I 950 50 .
I 700 100 .
""")
      }
    }

    "allow raise when facing a full raise" in {

          val dealer = Visual << """
100 F 0 0 100!0 0 1 2 3 4
I 250 100 RR100
N 0 125 AA
I 200 125 CA
N 0 200 AA
I 200 200 CA
"""

      dealer.raise(100) must beDealer("""
100 F 0 0 100!0 0 1 2 3 4
I 50 300 RR100
N 0 125 AA
I 200 125 CA
N 0 200 AA
I 200 200 CA
""")

    }

    "allow raise when not facing a full raise but hasn't acted" in {
          val dealer = Visual << """
4000 P 2 4 4000!0 0 1 2 3 4
I 1000 4000 CA
F 1000 0 FO
N 0 7500 AH
F 1000 2000 FO
I 16000 4000 .
"""

      dealer.raise(4000) must beDealer("""
4000 P 2 4 4000!0 0 1 2 3 4
I 1000 4000 CA
F 1000 0 FO
N 0 7500 AH
F 1000 2000 FO
I 8500 11500 RR4000
""")
    }

    "dont allow raise when not facing a full raise" in {

          val dealer = Visual << """
100 F 0 2 100!0 0 1 2 3 4
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
100 F 0 0 100!0 0 1 2 3 4
I 50 200 CA
N 0 125 AA
I 200 125 CA
N 0 200 AA
I 200 200 CA
"""

      Some(dealer.nextTurn) must beDealer("""
100 F 0 2 100!0 0 1 2 3 4
I 50 200 CA
N 0 125 AA
I 200 125 CA
N 0 200 AA
I 200 200 CA
""")
    }


    "have next round" in {
      val dealer = Visual << """
100 F 0 0 100!0 0 1 2 3 4
I 50 200 CA
N 0 125 AA
I 200 200 CA
N 0 200 AA
I 200 200 CA
"""

      Some(dealer.nextRound) must beDealer("""
100 T 0 2 100!300 0 2 3 4~625 0 1 2 3 4
I 50 0 .
O 0 0 .
I 200 0 .
O 0 0 .
I 200 0 .
""")
    }


    "have pots" in {

      "have pots after next round" in {

        val dealer = Visual << """
10 F 0 4 10!0 0 1 2 3 4
N 0 100 AF
N 0 75 AC
N 0 50 AC
N 0 25 AC
I 25 100 CA
"""

        Some(dealer.endRound) must beDealer("""
10 F 0 4 10!50 0 4~75 0 1 4~100 0 1 2 4~125 0 1 2 3 4
O 0 0 .
O 0 0 .
O 0 0 .
O 0 0 .
I 25 0 .
""")

      }

      "have pots after next round with more involved" in {

        val dealer = Visual << """
10 F 0 4 10!0 0 1 2 3 4
N 0 100 AF
N 0 75 AC
N 0 50 AC
N 0 25 AC
I 25 120 RR20
I 25 120 CA
"""

        Some(dealer.nextRound) must beDealer("""
10 T 0 4 10!40 4 5~75 0 4 5~100 0 1 4 5~125 0 1 2 4 5~150 0 1 2 3 4 5
O 0 0 .
O 0 0 .
O 0 0 .
O 0 0 .
I 25 0 .
I 25 0 .
""")

      }

      "have pots accumulate running pot" in {

        val dealer = Visual << """
10 F 0 4 10!100 0 1 2 3 4
N 0 100 AF
N 0 75 AC
N 0 50 AC
N 0 25 AC
I 25 120 RR20
I 25 120 CA
"""

        Some(dealer.nextRound) must beDealer("""
10 T 0 4 10!40 4 5~75 0 4 5~100 0 1 4 5~125 0 1 2 4 5~250 0 1 2 3 4 5
O 0 0 .
O 0 0 .
O 0 0 .
O 0 0 .
I 25 0 .
I 25 0 .
""")

      }


      "have pots accumulate running pot with folded stacks" in {

        val dealer = Visual << """
10 F 0 4 10!100 0 1 2 3 4
N 0 100 AF
N 0 75 AC
N 0 50 AC
N 0 25 AC
I 25 120 RR20
I 25 120 CA
F 25 80 FO
"""

        Some(dealer.nextRound) must beDealer("""
10 T 0 4 10!40 4 5~80 0 4 5~125 0 1 4 5~150 0 1 2 4 5~275 0 1 2 3 4 5
O 0 0 .
O 0 0 .
O 0 0 .
O 0 0 .
I 25 0 .
I 25 0 .
F 25 0 .
""")

      }

      "have pots accumulate running pot with two folded stacks" in {

        val dealer = Visual << """
10 F 0 4 10!100 0 1 2 3 4
N 0 100 AF
N 0 75 AC
N 0 50 AC
N 0 25 AC
I 25 120 RR20
I 25 120 CA
F 25 80 FO
F 25 110 FO
"""

        Some(dealer.nextRound) must beDealer("""
10 T 0 4 10!50 4 5~105 0 4 5~150 0 1 4 5~175 0 1 2 4 5~300 0 1 2 3 4 5
O 0 0 .
O 0 0 .
O 0 0 .
O 0 0 .
I 25 0 .
I 25 0 .
F 25 0 .
F 25 0 .
""")

      }
    }

    "distribute pots" in {

      "distribute one" in {
        val dealer = Visual << """
10 T 0 4 10!300 0 1 2 3 4 5
O 0 0 .
O 0 0 .
O 0 0 .
O 0 0 .
I 25 0 .
F 25 0 .
F 25 0 .
"""

        dealer.distributeOne must_== PotDistribution(300, List(4))

        Some(dealer.oneWinner) must beSome.like {
          case Winners(_, stacks) =>
            stacks(4) must_== Chips(325)
        }
      }

      "distribute all" in {
        val dealer = Visual << """
10 T 0 4 10!50 4 5~105 0 4 5~300 0 1 2 3 4 5
O 0 0 .
O 0 0 .
O 0 0 .
O 0 0 .
I 25 0 .
I 25 0 .
F 25 0 .
F 25 0 .
"""

        dealer.distributeAll(List(1, 2, 3, 4, 5, 6, 7, 8)) must_== List(
          PotDistribution(50, List(5)),
          PotDistribution(105, List(5)),
          PotDistribution(300, List(5))
        )
      }

      "distribute all with folded side pots" in {
        val dealer = Visual << """
10 T 0 4 10!50 4 5~105 0 4 5 7~300 0 1 2 3 4 5 6 7
O 0 0 .
O 0 0 .
O 0 0 .
O 0 0 .
I 25 0 .
I 25 0 .
F 25 0 .
F 25 0 .
"""

        dealer.distributeAll(List(1, 20, 3, 4, 5, 6, 7, 8)) must_== List(
          PotDistribution(50, List(5)),
          PotDistribution(105, List(5)),
          PotDistribution(300, List(1))
        )
      }

      "distribute all with equal hands" in {
        val dealer = Visual << """
10 T 0 4 10!50 4 5~105 0 4 5 7~300 0 1 2 3 4 5 6 7
O 0 0 .
O 0 0 .
O 0 0 .
O 0 0 .
I 25 0 .
I 25 0 .
F 25 0 .
F 25 0 .
"""

        dealer.distributeAll(List(1, 200, 3, 4, 56, 56, 7, 8)) must_== List(
          PotDistribution(50, List(4, 5)),
          PotDistribution(105, List(4, 5)),
          PotDistribution(300, List(1))
        )
      }
    }

    "have valid raises" in {

      "pot halfpot thirdpot" in {

        val dealer = Visual << """
10 F 0 1 10!100 0 1 2
N 0 30 AF
I 220 0 .
I 25 20 CA
"""

        dealer.validRaises must_== List(
          RegularRaise(10),
          PotRaise(180),
          HalfPotRaise(90),
          ThirdPotRaise(60)
        )
      }


      "not enough for pot raise" in {

        val dealer = Visual << """
10 F 0 1 12!100 0 1 2
N 0 30 AF
I 210 0 .
I 25 20 CA
"""

        dealer.validRaises must_== List(
          RegularRaise(12),
          HalfPotRaise(90),
          ThirdPotRaise(60)
        )
      }



      "not enough raise for third pot raise" in {

        val dealer = Visual << """
10 P 0 1 10!0 0 1 2
N 0 5 AF
I 300 0 .
I 25 10 CA
"""

        dealer.validRaises must_== List(
          RegularRaise(10),
          PotRaise(25),
          HalfPotRaise(12.5f)
        )
      }

      "not enough raise for min raise" in {

        val dealer = Visual << """
10 P 0 1 5!0 0 1 2
N 0 5 AF
I 3 0 .
I 25 10 CA
"""

        dealer.validRaises must_== List(
        )
      }
    }

  }

}
