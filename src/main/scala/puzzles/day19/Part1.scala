package puzzles.day19

import submarine.Scanner

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def totalBeacons: Int =
    Using.resource(Source.fromResource("day19.txt")) {
      file =>
        val scanners = Scanner.parseScanner(file.getLines())
        Scanner
          .matchAll(scanners.tail, Seq(scanners.head.copy(origin = (0, 0, 0))))
          .flatMap(_.beacons)
          .toSet
          .size
    }

  Console.println(f"The total number of beacons is $totalBeacons")
}
