package poker
package format

class VisualTest extends PokerTest {

  val f = Visual

  "The visual dealer formatter" should {
    "export dealer" in {
      f.addNewLines(f >> (makeDealer(List(100, 100, 100)))) must_== newDealerFormat
    }

    "import and export is non destructive" in {
      forall(examples) { example =>
        f.addNewLines(f >> (f << example)) must_== example
      }
    }
  }

  val newDealerFormat = """
P 0 0 2!0 0 1 2
I 100 0 .
I 99 1 .
I 98 2 .
"""

val examples = Seq(
  newDealerFormat,
"""
P 0 0 10!0 0 1 2
I 100 10 CA
I 100 10 CA
I 100 10 CH
""",
"""
P 0 1 10!0 0 1
I 80 20 RR100
I 90 10 CA
"""
)

}
