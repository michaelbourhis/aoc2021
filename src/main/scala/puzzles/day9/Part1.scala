package puzzles.day9

import cave.Smoke

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def sumOfRiskLevelsOfLowPoints: Int =
    Using.resource(Source.fromResource("day9.txt")) {
      file =>
        val finalStatus = file
          .getLines()
          .map(Smoke.parse)
          .foldLeft(Smoke.initStatus)((status, smokeLine) => {
            if (status.firstRow.isEmpty)
              status.copy(firstRow = smokeLine)
            else if (status.secondRow.isEmpty)
              status.copy(
                lowPoints = Smoke.findLowPoints(status.firstRow, nextSmokeLine = smokeLine),
                secondRow = smokeLine
              )
            else
              status.copy(
                lowPoints = status.lowPoints ++ Smoke.findLowPoints(status.secondRow, previousSmokeLine = status.firstRow, nextSmokeLine = smokeLine),
                firstRow = status.secondRow,
                secondRow = smokeLine
              )
          })
        val allLowPoints = Smoke.findLowPoints(finalStatus.secondRow, previousSmokeLine = finalStatus.firstRow) ++ finalStatus.lowPoints
        allLowPoints
          .map(_.riskLevel)
          .sum
    }

  Console.println(f"The sum of risk levels of all low points is $sumOfRiskLevelsOfLowPoints.")
}
