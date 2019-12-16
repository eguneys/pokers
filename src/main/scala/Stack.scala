package poker

case class Stack(role: StackRole, stack: Int, recentWager: Int, lastAction: Option[PlayerAct]) {


  def is(t: StackRole) = role == t

  def raise(to: Int, toCall: Int): Option[Stack] = {
    val total = to + toCall
    val newStack = stack - total
    val newWager = recentWager + total

    Some(copy(stack = newStack,
      recentWager = newWager,
      lastAction = Some(RegularRaise(to))))
  }

  def call(toCall: Int): Option[Stack] = {
    val total = toCall
    val newStack = stack - total
    val newWager = recentWager + total

    Some(copy(stack = newStack,
      recentWager = newWager,
      lastAction = Some(Call)))
  }

}
