package poker


sealed trait StackRole {
  val forsyth: Char
}

case object Involved extends StackRole {
  val forsyth = 'I'
}

case object Folded extends StackRole {
  val forsyth = 'F'
}

case object NewAllIn extends StackRole {
  val forsyth = 'N'
}

case object OldAllIn extends StackRole {
  val forsyth = 'O'
}

object StackRole {

  val all = List(Involved, Folded, NewAllIn, OldAllIn)

  val allByForsyth = all.map { round => round.forsyth -> round } toMap

  def forsyth(c: Char): Option[StackRole] = allByForsyth get c

}
