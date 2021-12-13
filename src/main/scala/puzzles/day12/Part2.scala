package puzzles.day12

import cave.{Cave, Pathing}

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def pathsVisitingSmallCavesAtMostOnceExceptOne: Long =
    Using.resource(Source.fromResource("day12.txt")) {
      file =>
        val mapOfTheCaves = Pathing.parse(
          file.getLines().toSeq
        )
        Pathing.pathToEnd(Seq(Cave("start")), mapOfTheCaves, canVisitSmallTwice = true)
          .size
    }

  Console.println(f"Possible paths with small caves visited at most once except one: $pathsVisitingSmallCavesAtMostOnceExceptOne.")
}
