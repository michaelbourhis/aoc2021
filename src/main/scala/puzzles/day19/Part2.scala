package puzzles.day19

import submarine.Scanner

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def largestManhattan: Int =
    Using.resource(Source.fromResource("day19.txt")) {
      file =>
        val scanners = Scanner.parseScanner(file.getLines())
        val relocatedScanners = Scanner.matchAll(scanners.tail, Seq(scanners.head.copy(origin = (0, 0, 0))))
        (0 until relocatedScanners.length - 1).flatMap(i =>
          (i + 1 until relocatedScanners.length).map(j => {
            math.abs(relocatedScanners(i).origin._1 - relocatedScanners(j).origin._1) +
              math.abs(relocatedScanners(i).origin._2 - relocatedScanners(j).origin._2) +
              math.abs(relocatedScanners(i).origin._3 - relocatedScanners(j).origin._3)
          }
          )
        ).max
    }

  Console.println(f"The largest Manhattan distance is $largestManhattan")
}
