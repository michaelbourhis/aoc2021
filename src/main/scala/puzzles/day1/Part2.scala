package puzzles.day1

import utils.Depth

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def numberOfIncreasesWithSlidingWindow: Int =
    Using.resource(Source.fromResource("day1.txt")) {
      file =>
        file
          .getLines()
          .sliding(3)
          .map(lines => lines.map(Depth.parse).sum)
          .sliding(2)
          .map(
            depths => {
              val previousDepth = depths.head
              val currentDepth = depths.last
              if (previousDepth < currentDepth) 1 else 0
            }
          ).sum
    }

  Console.println(f"Found $numberOfIncreasesWithSlidingWindow increases in measure using a sliding window.")
}
