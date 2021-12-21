package animals

import scala.annotation.tailrec

object LanternFish {
  type FishState = Map[Int, Long]
  val cycleLength = 6
  val cycleInitLength = 2

  def initCycle(representation: String): FishState =
    representation
      .split(",")
      .map(_.toInt)
      .groupBy(identity)
      .view.mapValues(_.length.toLong)
      .toMap

  @tailrec
  def reproduction(fishState: FishState, daysLeft: Int): FishState =
    if (daysLeft == 0)
      fishState
    else {
      val reproducingFish = fishState.get(0).map(numberOfFish =>
        Seq((cycleLength, numberOfFish), (cycleLength + cycleInitLength, numberOfFish))
      ).getOrElse(Seq.empty).toMap
      val otherFish = fishState.keys.filterNot(_ == 0).map(day => (day - 1, fishState(day)))
      val newFishState = reproducingFish ++ otherFish.map {
        case (day, numberOfFish) => day -> (numberOfFish + reproducingFish.getOrElse(day, 0L))
      }
      reproduction(newFishState, daysLeft - 1)
    }
}
