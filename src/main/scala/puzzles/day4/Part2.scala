package puzzles.day4

import submarine.Grid

import scala.io.Source
import scala.util.Using

object Part2 extends App {
  def finalScore: Int =
    Using.resource(Source.fromResource("day4.txt")) {
      file =>
        val bingoData = file.getLines()
        val numbersDrawn = bingoData.next().split(",")
        val grids = bingoData
          .sliding(6, 6)
          .map(_.tail)
          .map(Grid.parse)
          .toSeq
        val lastWinningGrid = Grid.drawUntilLastCompletes(grids, numbersDrawn)
        lastWinningGrid.map(_.score).get
    }

  Console.println(f"The last winning grid has a score of $finalScore.")
}
