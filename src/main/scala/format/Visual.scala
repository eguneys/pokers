package poker
package format


// blinds bettingRound button turnToAct lastFullRaise!runningPot~sidePot
// role stack recentWager lastAction|. 

// 100 (P|F|T|R) 0 0 100!100 0 1 2 3~50 0 1 2
// (I|F|O|N) 100 10 CA
// I 100 10 RR200
// I 100 10 .


object Visual {

  private val HeaderPattern = "(\\d+) (P|F|T|R) (\\d) (\\d) (\\d+)".r

  private val StackPattern = "(I|F|O|N) (\\d+) (\\d+) (\\.|\\w+)".r

  private val ActPattern = "(CA|CH|FO|RR|AA|AC|AH|AF)(\\d*)".r

  private def readAct(str: String) = str match {
    case ActPattern(act, "") =>
      PlayerAct.forsyth(act).get
    case ActPattern(act, raise) => RegularRaise(raise.toInt)
  }

  private def writeAct(act: PlayerAct) = act match {
    case r@RegularRaise(amount) => r.uci + amount
    case act => act.uci
  }

  def readStack(source: String): Stack = source match {
    case StackPattern(role, stack, recentWager, ".") =>
      Stack(StackRole forsyth role.charAt(0) get, stack.toInt, recentWager.toInt, None)
    case StackPattern(role, stack, recentWager, lastAction) =>
      Stack(StackRole forsyth role.charAt(0) get, stack.toInt, recentWager.toInt, Some(readAct(lastAction)))
  }

  def writeStack(stack: Stack): String = {
    stack.role.forsyth + " " + stack.stack + " " + stack.recentWager + " " + stack.lastAction.map(_.uci).getOrElse(".")
  }

  def readPot(source: String): Pot = {
    val header = source.split(' ')

    val wager = header.head.toInt
    val involved = header.tail.map(_.toInt).toList

    Pot(wager, involved)
  }

  def writePot(pot: Pot): String = {
    pot.wager +: pot.involved mkString " "
  }

  def <<(source: String): Dealer = {
    val lines = source.trim.lines.toList

    val headerPots = lines.head.split('!')
    val headerS = headerPots.head
    val pots = headerPots.drop(1).head.split('~').map(readPot).toList

    val stacks = lines.tail.map(readStack)

    val runningPot = pots.head
    val sidePots = pots.tail


    headerS match {
      case HeaderPattern(blinds, round, button, toAct, lastFullRaise) => Dealer(
        blinds.toInt,
        BettingRound forsyth round.charAt(0) get,
        button.toInt,
        toAct.toInt,
        lastFullRaise.toInt,
        stacks.toVector,
        runningPot,
        sidePots
      )
    }
  }


  def >>(dealer: Dealer): String = {

    val stacks = dealer.stacks.map (writeStack) mkString "\n"
    val pots = (dealer.runningPot +: dealer.sidePots).map(writePot) mkString "~"

    dealer.blinds + " " + dealer.round.forsyth + " " + dealer.button + " " + dealer.turnToAct + " " + dealer.lastFullRaise + "!" + pots + "\n" + stacks
  }

  def addNewLines(str: String) = "\n" + str + "\n"

}
