package animals

import scala.annotation.tailrec

case class DumboOctopus(energyLevel: Int, flashedInThisStep: Boolean = false, totalFlashes: Int = 0) {
  def canFlash: Boolean = energyLevel >= 10 && !flashedInThisStep

  def flash: DumboOctopus = this.copy(flashedInThisStep = true, totalFlashes = totalFlashes + 1)

  def increaseEnergyLevel: DumboOctopus = this.copy(energyLevel = energyLevel + 1)

  def resetAfterFlash: DumboOctopus = this.copy(energyLevel = 0, flashedInThisStep = false)
}

object DumboOctopus {
  type DumboOctopusesGrid = Seq[Seq[DumboOctopus]]
  type DumboOctopusesLine = Seq[DumboOctopus]

  def parse(representation: String): DumboOctopusesLine =
    representation.toCharArray.map(_.toString.toInt).map(DumboOctopus(_))

  def makeAStep(dumboOctopusesGrid: DumboOctopusesGrid): DumboOctopusesGrid = {
    val allEnergyIncreased = DumboOctopus.increaseTheirLevel(dumboOctopusesGrid)
    (0 to 9).foldLeft(allEnergyIncreased)((dumboOctopusesGridByRow, i) =>
      (0 to 9).foldLeft(dumboOctopusesGridByRow)((dumboOctopusesGridByColumn, j) => DumboOctopus.makeItFlash(dumboOctopusesGridByColumn, i, j))
    )
  }

  def resetAfterStep(dumboOctopusesGrid: DumboOctopusesGrid): DumboOctopusesGrid =
    dumboOctopusesGrid.map(dumboOctopusesLine => dumboOctopusesLine.map(dumbo => if (dumbo.flashedInThisStep) dumbo.resetAfterFlash else dumbo))

  def makeItFlash(dumboOctopusesGrid: DumboOctopusesGrid, i: Int, j: Int): DumboOctopusesGrid =
    if (dumboOctopusesGrid(i)(j).canFlash) {
      val gridAfterThisOneFlashed = dumboOctopusesGrid.updated(i, dumboOctopusesGrid(i).updated(j, dumboOctopusesGrid(i)(j).flash))
      getSurroundings(i, j).foldLeft(gridAfterThisOneFlashed)((flashingGrid, coordinates) => {
        val (i1, j1) = coordinates
        val increaseLevelOfNeighbour = flashingGrid.updated(i1, flashingGrid(i1).updated(j1, flashingGrid(i1)(j1).increaseEnergyLevel))
        makeItFlash(increaseLevelOfNeighbour, i1, j1)
      })
    } else
      dumboOctopusesGrid

  @tailrec
  def stopWhenAllFlashed(dumboOctopusesGrid: DumboOctopusesGrid, step: Int): Int = {
    val gridAfterAStep = DumboOctopus.makeAStep(dumboOctopusesGrid)
    if (gridAfterAStep.flatten.forall(_.flashedInThisStep)) step + 1
    else stopWhenAllFlashed(resetAfterStep(gridAfterAStep), step + 1)
  }

  def increaseTheirLevel(dumboOctopusesGrid: DumboOctopusesGrid): DumboOctopusesGrid =
    dumboOctopusesGrid.map(dumboOctopusesLine => dumboOctopusesLine.map(dumboOctopus => dumboOctopus.increaseEnergyLevel))

  def getSurroundings(i: Int, j: Int): Seq[(Int, Int)] =
    (-1 to 1).foldLeft(Seq.empty[(Int, Int)])((rowIndices, x) =>
      (-1 to 1).foldLeft(rowIndices)((columnIndices, y) => columnIndices ++ Seq((i + x, j + y)))
    ).filter {
      case (x, y) => x >= 0 && x <= 9 && y >= 0 && y <= 9 && (x != i || y != j)
    }
}