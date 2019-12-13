package poker

import org.specs2.matcher.{ /* Matcher, */ ValidationMatchers }
import org.specs2.mutable.Specification

import poker.format.{ Visual }

trait PokerTest extends Specification with ValidationMatchers {


  implicit def stringToSituationBuilder(str: String) = Situation(Visual << str)

  def makeDealer(blinds: Int, stacks: List[Int]): Dealer =
    Dealer.empty(blinds, 0, stacks)


}




