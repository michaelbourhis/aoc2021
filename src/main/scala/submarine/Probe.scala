package submarine

import scala.annotation.tailrec

case class TargetArea(minX: Int, minY: Int, maxX: Int, maxY: Int) {
  def reachedIt(probe: Probe): Boolean =
    probe.x >= minX && probe.x <= maxX && probe.y >= minY && probe.y <= maxY

  def missedIt(probe: Probe): Boolean =
    (minX <= 0 && probe.x < minX) || (maxX >= 0 && probe.x > maxX) || (probe.velocity.y <= 0 && probe.y < minY)
}

case class Velocity(x: Int, y: Int) {
  def move: Velocity =
    Velocity(
      if (x > 0) x - 1 else if (x < 0) x + 1 else 0,
      y - 1
    )
}

case class Probe(x: Int, y: Int, velocity: Velocity) {
  def move: Probe =
    Probe(
      x + velocity.x,
      y + velocity.y,
      velocity.move
    )
}

object Probe {
  def parseTargetArea(representation: String): TargetArea = {
    val targetAreaRegex = raw"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)".r
    representation match {
      case targetAreaRegex(minX, maxX, minY, maxY) => TargetArea(minX.toInt, minY.toInt, maxX.toInt, maxY.toInt)
    }
  }

  def velocityValues(targetArea: TargetArea): (Int, Int, Int, Int) = {
    val (startX, endX) =
      if (targetArea.minX <= 0 && targetArea.maxX >= 0) (targetArea.minX, targetArea.maxX)
      else if (targetArea.minX < 0) (targetArea.minX, -1)
      else (1, targetArea.maxX)
    val (startY, endY) = (
      targetArea.minY,
      optimumVelocity(targetArea)
    )
    (startX, endX, startY, endY)
  }

  def optimumVelocity(targetArea: TargetArea): Int = {
    val bottomOfTheTrench = targetArea.minY
    -1 - bottomOfTheTrench
  }

  @tailrec
  def trajectory(probe: Probe, targetArea: TargetArea, maxY: Int = 0): Option[Int] = {
    if (targetArea.missedIt(probe)) None
    else if (targetArea.reachedIt(probe)) Some(maxY)
    else {
      val probeMoves = probe.move
      val maxYAfterMove = if (probe.y > maxY) probe.y else maxY
      trajectory(probeMoves, targetArea, maxYAfterMove)
    }
  }
}
