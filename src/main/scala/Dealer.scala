package poker

case class Dealer(round: BettingRound,
  button: StackIndex,
  turnToAct: StackIndex,
  lastFullRaise: Chips,
  stacks: Vector[Stack],
  runningPot: Pot,
  sidePots: List[Pot]) {

  def diff(i: StackIndex): PlayerDiff = stacks(i).diff

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
  def newAllInStacks: Vector[Stack] = stacks.filter(_ is NewAllIn)

  def allActed: Boolean = stacks forall(_.lastAction.isDefined)
  def wagersEqualized: Boolean = {
    involvedStacks match {
      case head +: tail => 
        tail.forall(_.recentWager == head.recentWager)
      case _ => false
    }
  }

  def involvedsHigherThanNewAllIns: Boolean = {
    newAllInStacks.forall(newAllin =>
      involvedStacks.forall(_.recentWager > newAllin.recentWager))
  }


  def validRaises: List[Raise] = {
    val recentWagers = stacks.map(_.recentWager).foldLeft(Chips.empty)(_+_)
    val pot = runningPot.wager + recentWagers + toCall
    val halfPot = pot / 2
    val thirdPot = pot / 3
    val minRaise = lastFullRaise

    List(
      RegularRaise(minRaise),
      PotRaise(pot),
      HalfPotRaise(halfPot),
      ThirdPotRaise(thirdPot)
    ) filter (r => raise(r.to).isDefined)
  }

  private def highestWager: Chips = nonFoldStacks.foldLeft(Chips.empty) { (wager, stack) =>
    if (wager > stack.recentWager)
      wager
    else
      stack.recentWager
  }

  private def toCall: Chips = highestWager - toAct.recentWager

  private def toAct: Stack = stacks(turnToAct)

  private def updateToAct(f : Stack => Option[Stack]): Option[Dealer] = f(toAct) map { stack => copy(
    stacks = stacks.updated(turnToAct, stack)
  )}

  private def collectPots: Dealer = {

    def sliceAndBuildPot(foldeds: List[Chips], wagers: List[(Chips, StackIndex)], sidePots: List[Pot], runningPotWager: Chips = Chips.empty): List[Pot] = wagers match {
      case (Chips.empty, i) :: tail => sidePots
      case (wager, i) :: tail => {

        val lessFoldeds = foldeds.filter(_ <= wager)
        val moreFoldeds = foldeds.filterNot(_ <= wager)

        val newFoldeds = moreFoldeds.map(_ - wager)

        val foldedWager = lessFoldeds.foldLeft(Chips.empty)(_+_) + wager * moreFoldeds.length

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
      .filter(s => s.is(Folded) && s.recentWager > Chips.empty)
      .map(_.recentWager)
      .sorted
      .toList

    val pots = sliceAndBuildPot(foldeds, newAllins ++ involveds, Nil, runningPot.wager)

    pots match {
      case newRunningPot :: newSidePots =>
        copy(runningPot = newRunningPot,
          sidePots = newSidePots)
      case Nil =>
        this
    }
  }

  def distributeOne(): PotDistribution = {
    val allPots = runningPot.wager
    val involved = stacks.zipWithIndex
      .filter(_._1 is Involved)
      .map(_._2).toList

    PotDistribution(allPots, involved)
  }


  def distributeAll(handValues: List[HandValueMagic]): List[PotDistribution] = {
    val allPots = runningPot +: sidePots

    val foldeds = stacks.zipWithIndex
      .filter(_._1 is Folded)
      .map(_._2).toList

    allPots.map(_.distribute(handValues, foldeds))
  }

  def showdownWinner(handValues: List[HandValueMagic]): Winners = {
    val dists = distributeAll(handValues)
    val newStacks = dists.foldLeft(stacks) {
      case (stacks, dist) =>
        distributeToStacks(stacks, dist)
    }
    Winners(dists, newStacks.map(_.stack).toList)
  }

  def oneWinner: Winners = {
    val dists = List(distributeOne())
    val newStacks = dists.foldLeft(stacks) {
      case (stacks, dist) =>
        distributeToStacks(stacks, dist)
    }
    Winners(dists, newStacks.map(_.stack).toList)
  }

  private def distributeToStacks(stacks: Vector[Stack], dist: PotDistribution): Vector[Stack] = {
    val amount = dist.wager / dist.involved.length
    dist.involved.foldLeft(stacks) {
      case (stacks, i) =>
        stacks.updated(i, stacks(i).winPot(amount))
    }
  }

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
    if (toCall == Chips.empty)
      updateToAct(_.check())
    else
      None
  }

  def raise(to: Chips): Option[Dealer] = {
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
    val newStack = Chips.empty
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

  def empty(button: StackIndex, iStacks: List[Chips]) = {

    val sl = iStacks.length;

    val sb = (button + 1) % sl;
    val bb = (sb + 1) % sl;
    val toAct = (bb + 1) % sl;

    val SBChips = Chips(1)
    val BBChips = Chips(2)

    val lastFullRaise = BBChips

    val stacks = iStacks.zipWithIndex.map({
      case (stack, i) if i == sb  => Stack(Involved, stack - SBChips, SBChips, None)
      case (stack, i) if i == bb => Stack(Involved, stack - BBChips, BBChips, None)
      case (stack, _) => Stack(Involved, stack, Chips.empty, None)
    })

    val runningPot = Pot(Chips.empty, stacks.zipWithIndex.map(_._2))

    Dealer(Preflop,
      button,
      toAct,
      lastFullRaise,
      stacks.toVector,
      runningPot,
      Nil)
  }

}
