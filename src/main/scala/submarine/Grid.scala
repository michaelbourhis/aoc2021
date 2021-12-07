package submarine

import scala.annotation.tailrec

case class Cell(number: String, drawn: Boolean)

case class Grid(rows: Seq[Seq[Cell]], lastNumber: String = "") {
  def isCompleted: Boolean = {
    rows.exists(r => r.forall(_.drawn)) ||
      (0 to 4).map(i => rows.map(r => r(i))).exists(c => c.forall(_.drawn))
  }

  def score: Int =
    lastNumber.toInt * rows.flatten.filterNot(_.drawn).map(_.number.toInt).sum
}

object Grid {
  val gridSize = 5

  def parse(data: Seq[String]): Grid = {
    val rows = data.map(_.trim.split(" +"))
    Grid(
      rows.map(r => r.map(Cell(_, drawn = false)))
    )
  }

  def mark(num: String)(grid: Grid): Grid = {
    Grid(
      grid.rows.map(r => r.map(
        c => Cell(c.number, drawn = c.drawn || c.number.equals(num)))
      ),
      lastNumber = num
    )
  }

  def drawUntilOneCompletes(grids: Seq[Grid], drawnNumbers: Seq[String]): Grid = {
    val markedGrids = grids
      .map(Grid.mark(drawnNumbers.head))
    markedGrids
      .find(_.isCompleted)
      .getOrElse(Grid.drawUntilOneCompletes(markedGrids, drawnNumbers.tail))
  }

  @tailrec
  def drawUntilLastCompletes(grids: Seq[Grid], drawnNumbers: Seq[String], winningGrid: Option[Grid] = None): Option[Grid] = {
    if(drawnNumbers.isEmpty) return winningGrid
    val markedGrids = grids.map(Grid.mark(drawnNumbers.head))
    val winningGrids = markedGrids.filter(_.isCompleted)
    val lastWinningGrid = if(winningGrids.isEmpty) winningGrid else Some(winningGrids.last)
    Grid.drawUntilLastCompletes(markedGrids.filterNot(_.isCompleted), drawnNumbers.tail, winningGrid = lastWinningGrid)
  }
}
