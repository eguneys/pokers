package poker

sealed abstract class Status(val id: Int) extends Ordered[Status] {

  val name = s"${toString.head.toLower}${toString.tail}"

  def compare(other: Status) = id compare other.id

  def is(s: Status): Boolean = this == s

  def is(f: Status.type => Status): Boolean = is(f(Status))
  
}

object Status {

  case object Created extends Status(10)
  case object Started extends Status(20)
  case object OneWin extends Status(30)
  case object Showdown extends Status(40)
  case object Aborted extends Status(60)

}
