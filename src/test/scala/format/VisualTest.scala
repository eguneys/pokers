package poker
package format

class VisualTest extends PokerTest {

  val f = Visual

  "The visual dealer formatter" should {
    "export dealer" in {
      f.addNewLines(f >> (makeDealer(10, List(100, 100, 100)))) must_== newDealerFormat
    }

    "import and export is non destructive" in {
      forall(examples) { example =>
        f.addNewLines(f >> (f << example)) must_== example
      }
    }
  }

  val newDealerFormat = """
10.0 P 0 0 10.0!0.0 0 1 2
I 100.0 0.0 .
I 95.0 5.0 .
I 90.0 10.0 .
"""

val examples = Seq(
  newDealerFormat,
"""
10.0 P 0 0 10.0!0.0 0 1 2
I 100.0 10.0 CA
I 100.0 10.0 CA
I 100.0 10.0 CH
""",
"""
10.0 P 0 1 10.0!0.0 0 1
I 80.0 20.0 RR100.0
I 90.0 10.0 CA
"""
)

}
