package puzzles.day23

import animals.Amphipod

import scala.collection.SortedSet
import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def leastEnergyConsumed: Int =
    Using.resource(Source.fromResource("day23.txt")) {
      file =>
        val initialConfiguration = Amphipod.parseStartingConfiguration(file.getLines(), unfold = true)
        Amphipod.organise(SortedSet(initialConfiguration), Map((initialConfiguration.burrow, 0)))
    }

  Console.println(f"The least energy used to reorganize side rooms is: $leastEnergyConsumed.")
}
