package poker

class ChipsTest extends PokerTest {

  "chips" should {

    "divide rounded" in {
      val chips = Chips(10)

      chips /~ 3 must_== Chips(3.33f)
    }

  }
}
