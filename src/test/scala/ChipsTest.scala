package poker

class ChipsTest extends PokerTest {

  "chips" should {

    "divide rounded" in {
      val chips = Chips(10)

      (chips / 3).value must_== 3

      val chips1k = Chips(1000)

      (chips1k / 3).value must_== 333
    }
  }
}
