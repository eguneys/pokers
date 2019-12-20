package poker

case class Timestamp(value: Long) extends AnyVal {

  def -(o: Timestamp) = Centis.ofMillis(value - o.value)

  def +(o: Centis) = Timestamp(value + o.millis)

}

trait Timestamper {
  def now: Timestamp

  def toNow(ts: Timestamp) = now - ts
}

private[poker] object RealTimestamper extends Timestamper {

  def now = new Timestamp(System.currentTimeMillis)

}
