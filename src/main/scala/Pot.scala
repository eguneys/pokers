package poker

case class Pot(
  wager: Float,
  involved: List[StackIndex]) {

  def distribute(handValues: List[HandValueMagic], foldeds: List[StackIndex]): PotDistribution = {

    val realInvolved = involved.filterNot(foldeds.contains)

    val values = handValues
    val indexes = realInvolved

    // https://stackoverflow.com/a/59415108/3994249
    // mapping index - value
    // get max value
    // filter & map tuples
    val tuples = indexes.map(index => index -> values(index))
    val maxVal = tuples.reduce((t1, t2) => if(t1._2 > t2._2) t1 else t2)._2
    val maxIndices = tuples.filter(t => t._2 == maxVal).map(_._1)

    PotDistribution(wager, maxIndices)
  }
  
}
