package puzzles.day9

import cave.Smoke

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def totalSizeOfTop3WiderBasins: Int =
    Using.resource(Source.fromResource("day9.txt")) {
      file =>
        val finalStatus = file
          .getLines()
          .map(Smoke.parse)
          .foldLeft(Smoke.initStatus)((status, smokeLine) => {
            val smokeLineWithIndexes = smokeLine.zipWithIndex.map {
              case (smoke, i) => smoke.copy(rowIndex = status.currentRowIndex + 1, colIndex = i)
            }
            if (status.firstRow.isEmpty)
              status.copy(
                firstRow = smokeLineWithIndexes,
                allRows = Seq(smokeLineWithIndexes),
                currentRowIndex = 0
              )
            else if (status.secondRow.isEmpty)
              status.copy(
                lowPoints = Smoke.findLowPoints(status.firstRow, nextSmokeLine = smokeLineWithIndexes),
                secondRow = smokeLineWithIndexes,
                allRows = status.allRows ++ Seq(smokeLineWithIndexes),
                currentRowIndex = 1
              )
            else
              status.copy(
                lowPoints = status.lowPoints ++ Smoke.findLowPoints(status.secondRow, previousSmokeLine = status.firstRow, nextSmokeLine = smokeLineWithIndexes),
                firstRow = status.secondRow,
                secondRow = smokeLineWithIndexes,
                allRows = status.allRows ++ Seq(smokeLineWithIndexes),
                currentRowIndex = status.currentRowIndex + 1
              )
          })
        val allLowPoints = Smoke.findLowPoints(finalStatus.secondRow, previousSmokeLine = finalStatus.firstRow) ++ finalStatus.lowPoints
        allLowPoints
          .map(_.basin(finalStatus.allRows))
          .map(_.size)
          .sorted
          .reverse
          .take(3)
          .product
    }

  Console.println(f"The total size of the 3 wider basins is $totalSizeOfTop3WiderBasins.")
}
