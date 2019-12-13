package poker
package format


// blinds bettingRound button turnToAct allowRaiseUntil lastFullRaise!runningPot~sidePot
// role stack recentWager lastAction|. 

// 100 (P|F|T|R) 0 0 0 100!100 0 1 2 3~50 0 1 2
// (I|F|O|N) 100 10 CA
// I 100 10 RR200
// I 100 10 .


object Visual {

  private val HeaderPattern = "(\\d+) (P|F|T|R) (\\d) (\\d) (\\d) (\\d+)".r

  private val StackPattern = "(I|F|O|N) (\\d+) (\\d+) (\\.|\\w+)".r

  def readStack(source: String): Stack = source match {
    case StackPattern(role, stack, recentWager, ".") =>
      Stack(StackRole forsyth role.charAt(0) get, stack.toInt, recentWager.toInt, None)
    case StackPattern(role, stack, recentWager, lastAction) =>
      Stack(StackRole forsyth role.charAt(0) get, stack.toInt, recentWager.toInt, None)
  }

  def writeStack(stack: Stack): String = {
    stack.role.forsyth + " " + stack.stack + " " + stack.recentWager + " " + stack.lastAction.map(_.uci).getOrElse(".")
  }

  def <<(source: String): Dealer = {
    val lines = source.trim.lines.toList

    val headerPots = lines.head.split('!')
    val headerS = headerPots.head
    // val pots = headerPots.drop(1).head.split('~').map(readPot)

    val stacks = lines.tail.map(readStack)


    headerS match {
      case HeaderPattern(blinds, round, button, toAct, allowRaiseUntil, lastFullRaise) => Dealer(
        blinds.toInt,
        BettingRound forsyth round.charAt(0) get,
        button.toInt,
        toAct.toInt,
        allowRaiseUntil.toInt,
        lastFullRaise.toInt,
        stacks
      )
    }
  }


  def >>(dealer: Dealer): String = {

    val stacks = dealer.stacks.map (writeStack) mkString "\n"
    val pots = ""

    dealer.blinds + " " + dealer.round.forsyth + " " + dealer.button + " " + dealer.turnToAct + " " + dealer.allowRaiseUntil + " " + dealer.lastFullRaise + "!" + pots + "\n" + stacks
  }

  def addNewLines(str: String) = "\n" + str + "\n"

}
