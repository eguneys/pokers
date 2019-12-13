package poker

case class Dealer(blinds: Int,
  round: BettingRound,
  button: StackIndex,
  turnToAct: StackIndex,
  allowRaiseUntil: StackIndex,
  lastFullRaise: Int,
  // sidePots: List[Pot],
  stacks: List[Stack]) {

  lazy val SB: StackIndex = (button + 1) % stacks.length
  lazy val BB: StackIndex = (SB + 1) % stacks.length

  def newAllIns: Int = stacks count(_.role == NewAllIn)
  def oldAllIns: Int = stacks count(_.role == OldAllIn)

  def allIns = newAllIns + oldAllIns
  def folds: Int = stacks count(_.role == Folded)
  def involveds: Int = stacks count(_.role == Involved)

  def noneInvolved = involveds == 0
  def oneInvolved = involveds == 1
  def allInsExists = allIns > 0

  def allActed: Boolean = stacks forall(!_.lastAction.isDefined)
  def wagersEqualized: Boolean = {
    val involvedStacks = stacks
      .filter(_.role==Involved)

    involvedStacks match {
      case head :: tail => 
        tail.forall(_.recentWager == head.recentWager)
      case _ => false
    }
  }

  // def PotDistribution distributeOne()
  // def List[PotDistribution] distributeAll(List[HandValueMagic] handValues)

    // def Option[Dealer] nextRound()
    // def Option[Dealer] nextTurn()

    // def Option[Dealer] check()
    // def Option[Dealer] raise()
    // def Option[Dealer] fold()
    // def Option[Dealer] call()
    // def Option[Dealer] allin()
  }


case object Dealer {

  def empty(blinds: Int, button: StackIndex, iStacks: List[Int]) = {

    val sl = iStacks.length;

    val sb = (button + 1) % sl;
    val bb = (sb + 1) % sl;
    val toAct = (bb + 1) % sl;
    val allowRaiseUntil = bb;
    val lastFullRaise = blinds;

    val stacks = iStacks.zipWithIndex.map({
      case (stack, i) if i == sb  => Stack(Involved, stack, blinds / 2, None)
      case (stack, i) if i == bb => Stack(Involved, stack, blinds, None)
      case (stack, _) => Stack(Involved, stack, 0, None)
    })

    Dealer(blinds,
      Preflop,
      button,
      toAct,
      allowRaiseUntil,
      lastFullRaise,
      stacks)
  }

}
