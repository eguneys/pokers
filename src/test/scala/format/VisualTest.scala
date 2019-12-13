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
10 P 0 0 2 10!
I 100 0 .
I 100 5 .
I 100 10 .
"""

val examples = Seq(
  newDealerFormat,
"""
10 P 0 0 2 10!
I 100 10 CA
I 100 10 CA
I 100 10 CH
"""
)

}
