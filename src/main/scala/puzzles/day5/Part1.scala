package puzzles.day5

import vents.Line

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  val dangerLevel = 2

  def numberOfDangerousPoints: Int =
    Using.resource(Source.fromResource("day5.txt")) {
      file =>
        file
          .getLines()
          .map(Line.parseHorizontalAndVertical)
          .filterNot(_.points.isEmpty)
          .foldLeft(Line.initMapOfPoints)(Line.fillMapOfPoints)
          .dangerousPoints(dangerLevel)
          .size
    }

  Console.println(s"There are $numberOfDangerousPoints with at least $dangerLevel lines overlap.")
}
