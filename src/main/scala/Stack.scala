package poker

case class Stack(role: StackRole, stack: Int, recentWager: Int, lastAction: Option[PlayerAct]) {


  def is(t: StackRole) = role == t

  def allin(newStack: Int, newWager: Int, allInAct: PlayerAct): Option[Stack] = {
    Some(copy(NewAllIn, newStack, newWager, Some(allInAct)))
  }

  def fold(): Option[Stack] = {
    Some(copy(Folded, stack, recentWager, Some(Fold)))
  }

  def raise(to: Int, toCall: Int): Option[Stack] = {
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

  def call(toCall: Int): Option[Stack] = {
    val total = toCall
    val newStack = stack - total
    val newWager = recentWager + total

    if (newStack <= 0)
      None
    else
      Some(copy(stack = newStack,
        recentWager = newWager,
        lastAction = Some(Call)))
  }

  def check(): Option[Stack] = {
    Some(copy(lastAction = Some(Check)))
  }
}
