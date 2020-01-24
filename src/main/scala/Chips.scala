package poker

case class Chips(value: Int) extends Ordered[Chips] {

  def +(o: Chips) = Chips(value + o.value)
  def -(o: Chips) = Chips(value - o.value)
  def /(v: Int) = Chips(value / v)
  def *(v: Int) = Chips(value * v)

  def compare(o: Chips) = {
    value.compare(o.value)
  }

  override def toString: String = value.toString
}

object Chips {

  val empty = Chips(0)
  
}
