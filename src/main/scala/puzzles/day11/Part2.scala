package puzzles.day11

import animals.DumboOctopus

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def firstStepWhenAllFlash: Long =
    Using.resource(Source.fromResource("day11.txt")) {
      file =>
        val initialState = file
          .getLines()
          .toSeq
          .map(DumboOctopus.parse)
        DumboOctopus.stopWhenAllFlashed(initialState, 0)
    }

  Console.println(f"Dumbo octopuses will flash the first time all together at step $firstStepWhenAllFlash.")
}
