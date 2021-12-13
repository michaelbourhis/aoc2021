package puzzles.day11

import animals.DumboOctopus

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def numberOfFlashesAfter100Steps: Long =
    Using.resource(Source.fromResource("day11.txt")) {
      file =>
        val initialState = file
          .getLines()
          .toSeq
          .map(DumboOctopus.parse)
        (1 to 100).foldLeft(initialState)((dumboOctopusesGrid, _) => {
          val gridAfterAStep = DumboOctopus.makeAStep(dumboOctopusesGrid)
          DumboOctopus.resetAfterStep(gridAfterAStep)
        })
          .flatten
          .map(_.totalFlashes)
          .sum
    }

  Console.println(f"The dumbo octopuses have flashed $numberOfFlashesAfter100Steps times after 100 steps.")
}
