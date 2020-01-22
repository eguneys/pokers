package poker

case class Stack(role: StackRole, stack: Chips, recentWager: Chips, lastAction: Option[PlayerAct]) {

  def diff: PlayerDiff = PlayerDiff(stack, recentWager, role)

  def is(t: StackRole) = role == t

  def acted: Boolean = lastAction.isDefined

  def allin(newStack: Chips, newWager: Chips, allInAct: PlayerAct): Option[Stack] = {
    Some(copy(NewAllIn, newStack, newWager, Some(allInAct)))
  }

  def fold(): Option[Stack] = {
    Some(copy(Folded, stack, recentWager, Some(Fold)))
  }

  def raise(to: Chips, toCall: Chips): Option[Stack] = {
    val total = to + toCall
    val newStack = stack - total
    val newWager = recentWager + total

    if (newStack <= Chips.empty)
      None
    else
    Some(copy(stack = newStack,
      recentWager = newWager,
      lastAction = Some(RegularRaise(to))))
  }

  def call(toCall: Chips): Option[Stack] = {
    val total = toCall
    val newStack = stack - total
    val newWager = recentWager + total

    if (newStack <= Chips.empty || total == Chips.empty)
      None
    else
      Some(copy(stack = newStack,
        recentWager = newWager,
        lastAction = Some(Call)))
  }

  def check(): Option[Stack] = {
    Some(copy(lastAction = Some(Check)))
  }

  def collectWager: Stack = {
    val s1 = if (role == NewAllIn)
      copy(role = OldAllIn)
    else
      this

    s1.copy(recentWager = Chips.empty, lastAction = None)
  }

  def winPot(amount: Chips): Stack = copy(stack = stack + amount)
}
