package poker

case class Stack(role: StackRole, stack: Int, recentWager: Int, lastAction: Option[PlayerAct]) {


  def is(t: StackRole) = role == t

}