package poker

class DealerTest extends PokerTest {

  "a dealer" should {

    val dealer = makeDealer(100, List(100, 100, 100))

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

  }

}
