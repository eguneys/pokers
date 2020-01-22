package poker

case class PlayerDiff(
  newStack: Chips,
  newWager: Chips,
  newRole: StackRole)
