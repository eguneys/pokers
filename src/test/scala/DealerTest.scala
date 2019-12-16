package poker

import format.Visual.addNewLines

class DealerTest extends PokerTest {

  "a dealer" should {

    val dealer = makeDealer(100, List(1000, 1000, 1000))

    "have involved players" in {
      dealer.stacks.map(_.role) must_== List(Involved, Involved, Involved)
    }

    "have blinds, turnToAct etc." in {
      dealer.button must_== 0
      dealer.SB must_== 1
      dealer.BB must_== 2
      dealer.turnToAct must_== 0

      dealer.allowRaiseUntil must_== 2
      dealer.lastFullRaise must_== 100
    }

    "have player acts" in {

      "not allow raise less than last full raise" in {
        dealer.raise(0) must beNone
        dealer.raise(99) must beNone
      }

      "allow raise at least last full raise" in {
        dealer.raise(100) must beSome.like {
          case d => addNewLines(d.visual) must_== """
100 P 0 0 2 100!
I 800 200 RR100
I 950 50 .
I 900 100 .
"""
        }
      }

    }


  }

}
