package puzzles.day4

import submarine.Grid

import scala.io.Source
import scala.util.Using

object Part1 extends App {
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
        val winningGrid = Grid.drawUntilOneCompletes(grids, numbersDrawn)
        winningGrid.score
    }

  Console.println(f"The winning grid has a score of $finalScore.")
}
