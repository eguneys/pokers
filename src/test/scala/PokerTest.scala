package poker

import org.specs2.matcher.{ /* Matcher, */ ValidationMatchers }
import org.specs2.mutable.Specification

trait PokerTest extends Specification with ValidationMatchers {


  def makeDealer(blinds: Int, stacks: List[Int]): Dealer =
    Dealer.empty(blinds, 0, stacks)

  

}




