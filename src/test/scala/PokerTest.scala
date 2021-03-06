package poker

import org.specs2.matcher.{ Matcher, ValidationMatchers }
import org.specs2.mutable.Specification
import scalaz.{ Validation => V }
import V.FlatMap._

import poker.format.{ Visual }

trait PokerTest extends Specification with ValidationMatchers {

  implicit def intToChips(value: Int): Chips = Chips(value)


  implicit def stringToSituationBuilder(str: String) = {
    val dealer = Visual << str
    Situation(dealer, HandDealer.shuffled(dealer.stacks.length))
  }

  def makeDealer(stacks: List[Chips]): Dealer =
    Dealer.empty(0, stacks)

  implicit def richGame(game: Game) = new {

    def playMoves(moves: PlayerAct*): Valid[Game] = playMoveList(moves)


    def playMoveList(moves: Iterable[PlayerAct]): Valid[Game] = {

      val vg = moves.foldLeft(V.success(game): Valid[Game]) { (vg, move) =>
        val ng = vg flatMap { g =>
          g(move) map (_._1)
        }
        ng
      }
      vg
    }

  }

  def haveDealerAct(f: PartialFunction[DealerAct, Boolean]): Matcher[Valid[(Game, Move)]] = beSuccess.like {
    case (_, move) =>
      f(move.dealerAct)
  }

  def beGame(visual: String): Matcher[Valid[Game]] = beSuccess.like {
    case g => g.dealer.visual must_== (Visual << visual).visual
  }

  def beDealer(visual: String): Matcher[Option[Dealer]] = beSome.like {
    case d => Visual.addNewLines(d.visual) must_== visual
  }

  def bePoss(acts: PlayerAct*): Matcher[Valid[Game]] = beSuccess.like {
    case g => g.situation.possibleActs must contain(exactly(acts:_*))
  }

}




