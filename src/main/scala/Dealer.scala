package poker

case class Dealer(blinds: Int,
  round: BettingRound,
  button: StackIndex,
  turnToAct: StackIndex,
  lastFullRaise: Int,
  stacks: Vector[Stack],
  runningPot: Pot,
  sidePots: List[Pot]) {

  lazy val SB: StackIndex = (button + 1) % stacks.length
  lazy val BB: StackIndex = (SB + 1) % stacks.length

  private def nextInvolvedAfter(after: StackIndex): StackIndex = {
    (stacks.zipWithIndex.drop(after + 1) ++ stacks.zipWithIndex).find(_._1.is(Involved)).get._2
  }

  def nextToAct: StackIndex = nextInvolvedAfter(turnToAct)
  def firstToAct: StackIndex = nextInvolvedAfter(button)

  def newAllIns: Int = stacks count(_ is NewAllIn)
  def oldAllIns: Int = stacks count(_ is OldAllIn)

  def allIns = newAllIns + oldAllIns
  def folds: Int = stacks count(_ is Folded)
  def involveds: Int = stacks count(_ is Involved)

  def noneInvolved = involveds == 0
  def oneInvolved = involveds == 1
  def allInsExists = allIns > 0

  def nonFoldStacks: Vector[Stack] = stacks.filterNot(_ is Folded)
  def involvedStacks: Vector[Stack] = stacks.filter(_ is Involved)

  def allActed: Boolean = stacks forall(_.lastAction.isDefined)
  def wagersEqualized: Boolean = {
    involvedStacks match {
      case head +: tail => 
        tail.forall(_.recentWager == head.recentWager)
      case _ => false
    }
  }

  private def highestWager: Int = nonFoldStacks.foldLeft(0) { (wager, stack) =>
    if (wager > stack.recentWager)
      wager
    else
      stack.recentWager
  }

  private def toCall: Int = highestWager - toAct.recentWager

  private def toAct: Stack = stacks(turnToAct)

  private def updateToAct(f : Stack => Option[Stack]): Option[Dealer] = f(toAct) map { stack => copy(
    stacks = stacks.updated(turnToAct, stack)
  )}

  private def collectPots: Dealer = {

    def sliceAndBuildPot(foldeds: List[Int], wagers: List[(Int, StackIndex)], sidePots: List[Pot], runningPotWager: Int = 0): List[Pot] = wagers match {
      case (0, i) :: tail => sidePots
      case (wager, i) :: tail => {

        val lessFoldeds = foldeds.filter(_ <= wager)
        val moreFoldeds = foldeds.filterNot(_ <= wager)

        val newFoldeds = moreFoldeds.map(_ - wager)

        val foldedWager = lessFoldeds.sum + wager * moreFoldeds.length

        val newPot = Pot(
          foldedWager +
          runningPotWager +
          wager * (tail.length + 1), 
          (i +: tail.map(_._2)).sorted)
        val newWagers = tail.map(t => t._1 - wager -> t._2)
        sliceAndBuildPot(newFoldeds, newWagers, newPot +: sidePots)
      }
      case _ => sidePots
    }

    val newAllins = stacks.zipWithIndex
      .filter(_._1 is NewAllIn)
      .map(t => t._1.recentWager -> t._2)
      .sortBy(_._1)
      .toList

    val involveds = stacks.zipWithIndex
      .filter(_._1 is Involved)
      .map(t => t._1.recentWager -> t._2)
      .toList

    val foldeds = stacks
      .filter(s => s.is(Folded) && s.recentWager > 0)
      .map(_.recentWager)
      .sorted
      .toList

    val pots = sliceAndBuildPot(foldeds, newAllins ++ involveds, Nil, runningPot.wager)

    val newRunningPot = pots.head
    val newSidePots = pots.tail

    copy(runningPot = newRunningPot,
      sidePots = newSidePots)
  }

  // def PotDistribution distributeOne()
  // def List[PotDistribution] distributeAll(List[HandValueMagic] handValues)

  def endRound: Dealer = {
    collectPots.copy(
      stacks = stacks.map(_.collectWager)
    )
  }

  def nextRound: Dealer = {
    collectPots.copy(
      round = round.next,
      turnToAct = firstToAct,
      stacks = stacks.map(_.collectWager))
  }

  def nextTurn: Dealer = copy(turnToAct = nextToAct)

  def call(): Option[Dealer] = {
    updateToAct(_.call(toCall))
  }

  def check() :Option[Dealer] = {
    if (toCall != 0)
      None
    else
      updateToAct(_.check())
  }

  def raise(to: Int): Option[Dealer] = {
    if (to < lastFullRaise || (toAct.acted && toCall < lastFullRaise))
      None
    else {
      for {
        d1 <- updateToAct(_.raise(to, toCall))
        d2 = d1.copy(lastFullRaise = to)
      } yield d2
    }
  }

  def fold(): Option[Dealer] = {
    updateToAct(_.fold())
  }

  def allin(): Option[Dealer] = {

    val total = toAct.stack
    val newStack = 0
    val newWager = toAct.recentWager + total

    val raiseTo = total - toCall

    val allInAct = if (total < toCall)
      AllInCall
    else if (raiseTo < lastFullRaise)
      AllInHalfRaise
    else
      AllInFullRaise

    val newLastFullRaise = if (allInAct == AllInFullRaise)
      raiseTo
    else
      lastFullRaise

    for {
      d1 <- updateToAct(_.allin(newStack, newWager, allInAct))
      d2 = d1.copy(lastFullRaise = newLastFullRaise)
    } yield d2
  }

  def visual = format.Visual >> this

  override def toString = visual

}


case object Dealer {

  def empty(blinds: Int, button: StackIndex, iStacks: List[Int]) = {

    val sl = iStacks.length;

    val sb = (button + 1) % sl;
    val bb = (sb + 1) % sl;
    val toAct = (bb + 1) % sl;
    val lastFullRaise = blinds;

    val stacks = iStacks.zipWithIndex.map({
      case (stack, i) if i == sb  => Stack(Involved, stack - blinds / 2, blinds / 2, None)
      case (stack, i) if i == bb => Stack(Involved, stack - blinds, blinds, None)
      case (stack, _) => Stack(Involved, stack, 0, None)
    })

    val runningPot = Pot(0, stacks.zipWithIndex.map(_._2))

    Dealer(blinds,
      Preflop,
      button,
      toAct,
      lastFullRaise,
      stacks.toVector,
      runningPot,
      Nil)
  }

}
