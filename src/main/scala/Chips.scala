package poker

case class Chips(value: Float) extends Ordered[Chips] {

  def +(o: Chips) = Chips(value + o.value)
  def -(o: Chips) = Chips(value - o.value)
  def /(v: Int) = Chips(value / v)
  def *(v: Int) = Chips(value * v)

  def /~(v: Int) = Chips(roundUp(value / v))

  def rounded = (value * 1000).toInt / 1000f

  private def roundUp(n: Float): Float = (n / 0.005).toInt * 0.005f;

  def compare(o: Chips) = {
    rounded.compare(o.rounded)
  }

  override def toString = if (rounded==rounded.toInt) 
    rounded.toInt.toString
  else
    rounded.toString

}

object Chips {

  val empty = Chips(0)
  
}
