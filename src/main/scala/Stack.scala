package poker

case class Stack(role: StackRole, stack: Float, recentWager: Float, lastAction: Option[PlayerAct]) {

  def diff: PlayerDiff = PlayerDiff(stack, recentWager, role)

  def is(t: StackRole) = role == t

  def acted: Boolean = lastAction.isDefined

  def allin(newStack: Float, newWager: Float, allInAct: PlayerAct): Option[Stack] = {
    Some(copy(NewAllIn, newStack, newWager, Some(allInAct)))
  }

  def fold(): Option[Stack] = {
    Some(copy(Folded, stack, recentWager, Some(Fold)))
  }

  def raise(to: Float, toCall: Float): Option[Stack] = {
    val total = to + toCall
    val newStack = stack - total
    val newWager = recentWager + total

    if (newStack <= 0)
      None
    else
    Some(copy(stack = newStack,
      recentWager = newWager,
      lastAction = Some(RegularRaise(to))))
  }

  def call(toCall: Float): Option[Stack] = {
    val total = toCall
    val newStack = stack - total
    val newWager = recentWager + total

    if (newStack <= 0 || total == 0)
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

    s1.copy(recentWager = 0, lastAction = None)
  }

  def winPot(amount: Float): Stack = copy(stack = stack + amount)
}
