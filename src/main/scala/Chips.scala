package poker

case class Chips(value: Float) extends Ordered[Chips] {

  def +(o: Chips) = Chips(value + o.value)
  def -(o: Chips) = Chips(value - o.value)
  def /(v: Int) = Chips(value / v)
  def *(v: Int) = Chips(value * v)

  def compare(o: Chips) = value.compare(o.value)

  override def toString = {
    val rounded = (value * 1000).toInt / 1000
    if (value == rounded) f"$value%1.0f"
    else f"$value%1.4f"
  }

}

object Chips {

  val empty = Chips(0)
  
}
