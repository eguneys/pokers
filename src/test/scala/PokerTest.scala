package poker

import org.specs2.matcher.{ Matcher, ValidationMatchers }
import org.specs2.mutable.Specification
import scalaz.{ Validation => V }
import V.FlatMap._

import poker.format.{ Visual }

trait PokerTest extends Specification with ValidationMatchers {


  implicit def stringToSituationBuilder(str: String) = Situation(Visual << str)

  def makeDealer(blinds: Int, stacks: List[Int]): Dealer =
    Dealer.empty(blinds, 0, stacks)

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

  def beGame(visual: String): Matcher[Valid[Game]] = beSuccess.like {
    case g => g.dealer.visual must_== (Visual << visual).visual
  }

  def beDealer(visual: String): Matcher[Option[Dealer]] = beSome.like {
    case d => Visual.addNewLines(d.visual) must_== visual
  }


}




