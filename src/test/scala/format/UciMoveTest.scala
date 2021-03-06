package poker
package format

class UciMoveTest extends PokerTest {

  "act encoding" should {

    "be reflexive" in {
      val move = Uci.Move("FO").get
      move.playerAct must_== Fold

      val move2 = Uci.Move("RR100").get
      move2.playerAct must_== RegularRaise(100)

      Uci.Move("TR2.333") must beSome.like {
        case m =>
          m.playerAct must_== RegularRaise(2)
      }

    }

  }
  
}
