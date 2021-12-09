package animals

import scala.annotation.tailrec

object Crab {
  def initialPositions(representation: String): Map[Int, Int] =
    representation.split(",").map(_.toInt).groupBy(identity).view.mapValues(_.length).toMap

  def optimalPositionToAlign(positions: Map[Int, Int], expensiveMove: Boolean = false): Int = {
    val sortedPositions = positions.keys.toSeq.sorted
    findMinimumPosition(positions, sortedPositions, sortedPositions.head + (sortedPositions.last - sortedPositions.head) / 2, sortedPositions.head, sortedPositions.last, expensiveMove)
  }

  @tailrec
  def findMinimumPosition(positions: Map[Int, Int], sortedPositions: Seq[Int], testedPosition: Int, minPosition: Int, maxPosition: Int, expensiveMove: Boolean = false): Int = {
    if (minPosition >= maxPosition)
      testedPosition
    else {
      val fuelForTested = totalFuelNeededToAlignTo(positions, testedPosition, expensiveMove)
      if (fuelForTested > totalFuelNeededToAlignTo(positions, testedPosition - 1, expensiveMove))
        findMinimumPosition(positions, sortedPositions, minPosition + (testedPosition - minPosition) / 2, minPosition, testedPosition, expensiveMove)
      else if (fuelForTested > totalFuelNeededToAlignTo(positions, testedPosition + 1, expensiveMove))
        findMinimumPosition(positions, sortedPositions, minPosition + (maxPosition - testedPosition + 1) / 2, testedPosition, maxPosition, expensiveMove)
      else
        testedPosition
    }
  }


  def totalFuelNeededToAlignTo(positions: Map[Int, Int], targetPosition: Int, expensiveMove: Boolean = false): Int =
    if (expensiveMove)
      positions.map {
        case (position, howManyAtThisPosition) =>
          (0 to (position - targetPosition).abs).sum * howManyAtThisPosition
      }.sum
    else
      positions.map {
        case (position, howManyAtThisPosition) =>
          (position - targetPosition).abs * howManyAtThisPosition
      }.sum

}
