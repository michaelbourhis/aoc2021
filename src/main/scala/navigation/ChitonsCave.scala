package navigation

import scala.annotation.tailrec
import scala.collection.SortedSet
import scala.collection.immutable.HashSet

case class CavePosition(x: Int, y: Int, density: Int) {
  def coordinates: (Int, Int) = (x, y)

  def toNode: Node = Node(x, y)

  def toNodeWithRisk(risk: Long): Node = Node(x, y, risk + density)
}

case class Node(x: Int, y: Int, riskLevel: Long = Long.MaxValue) extends Ordered[Node] {
  def coordinates: (Int, Int) = (x, y)

  def compare(that: Node): Int = {
    if (this.riskLevel < that.riskLevel) -1
    else if (this.riskLevel > that.riskLevel) 1
    else {
      if (this.x < that.x) -1
      else if (this.x > that.x) 1
      else {
        if (this.y < that.y) -1
        else if (this.y > that.y) 1
        else 0
      }
    }
  }
}

object ChitonsCave {
  type Grid = Seq[Seq[CavePosition]]
  type Remaining = SortedSet[Node]
  type Undiscovered = HashSet[(Int, Int)]

  def parse(representations: Iterator[String]): Grid =
    representations.zipWithIndex.map {
      case (representation, x) => representation.toCharArray.toSeq.zipWithIndex.map {
        case (c, y) => CavePosition(x, y, c.toString.toInt)
      }
    }.toSeq

  def init(grid: Grid, multiplier: Int): (Remaining, Undiscovered) =
    (
      SortedSet(Node(0, 0, riskLevel = 0L)),
      initUndiscoveredPoints(multiplier, grid.length, grid.head.length)
    )

  def initUndiscoveredPoints(multiplier: Int, xSize: Int, ySize: Int): Undiscovered = {
    (0 until multiplier).foldLeft(HashSet.empty[(Int, Int)])((undiscovered, i) =>
      (0 until multiplier).foldLeft(undiscovered)((undiscoveredPartial, j) =>
        undiscoveredPartial ++ (0 until xSize).foldLeft(HashSet.empty[(Int, Int)])((xCoordinates, x) =>
          (0 until ySize).foldLeft(xCoordinates)((yCoordinates, y) =>
            if (x + i * xSize == 0 && y + j * ySize == 0) yCoordinates
            else yCoordinates ++ HashSet((x + i * xSize, y + j * ySize))
          )
        )
      )
    )
  }

  @tailrec
  def findPath(remaining: SortedSet[Node], undiscovered: Undiscovered, grid: Grid, multiplier: Int): Long = {
    val maxX = grid.length
    val maxY = grid.head.length
    val nextPosition = remaining.head
    if (nextPosition.x == multiplier * maxX - 1 && nextPosition.y == multiplier * maxY - 1) nextPosition.riskLevel
    else {
      val neighbours = getSurroundings(grid, nextPosition.x, nextPosition.y, maxX, maxY, multiplier)
      val (remainingWithNewOnes, updatedUndiscovered) = updateUndiscovered(remaining.drop(1), undiscovered, neighbours)
      val updatedRemaining = updateDistance(nextPosition, neighbours, remainingWithNewOnes)
      findPath(updatedRemaining, updatedUndiscovered, grid, multiplier)
    }
  }

  def getSurroundings(allPositions: Seq[Seq[CavePosition]], i: Int, j: Int, maxI: Int, maxJ: Int, multiplier: Int): Seq[CavePosition] =
    Seq(
      (i + 1, j),
      (i, j + 1),
      (i, j - 1),
      (i - 1, j)
    ).filter {
      case (x, y) => x >= 0 && x < maxI * multiplier && y >= 0 && y < maxJ * multiplier
    }.map {
      case (x, y) if x >= maxI || y >= maxJ => CavePosition(x, y, increaseDensity(allPositions(x % maxI)(y % maxJ).density, x / maxI + y / maxJ))
      case (x, y) => allPositions(x)(y)
    }

  def increaseDensity(density: Int, added: Int): Int = 1 + (density + added - 1) % 9

  def updateUndiscovered(remaining: Remaining, undiscovered: Undiscovered, neighbours: Seq[CavePosition]): (Remaining, Undiscovered) = {
    val newOnes = neighbours.filter(n => undiscovered.contains(n.coordinates))
    if (newOnes.isEmpty) (remaining, undiscovered)
    else
      (
        remaining ++ newOnes.map(_.toNode),
        undiscovered.filterNot(u => newOnes.exists(n => n.coordinates == u))
      )
  }

  def updateDistance(from: Node, neighbours: Seq[CavePosition], remaining: Remaining): Remaining = {
    val neighboursToUpdate = neighbours.filter(to =>
      from.riskLevel + to.density < remaining.find(_.coordinates == to.coordinates).map(_.riskLevel).getOrElse(-1L)
    )
    remaining.filter(r => neighboursToUpdate.forall(_.coordinates != r.coordinates)) ++ neighboursToUpdate.map(_.toNodeWithRisk(from.riskLevel))
  }
}
