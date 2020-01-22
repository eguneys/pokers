package poker

case class Chips(value: Float) extends Ordered[Chips] {

  def +(o: Chips) = Chips(value + o.value)
  def -(o: Chips) = Chips(value - o.value)
  def /(v: Int) = Chips(value / v)
  def *(v: Int) = Chips(value * v)

  def rounded = (value * 1000).toInt / 1000f

  def compare(o: Chips) = {
    rounded.compare(o.rounded)
  }

  override def toString = rounded.toString

}

object Chips {

  val empty = Chips(0)
  
}
