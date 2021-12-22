package submarine

import scala.annotation.tailrec

case class Cuboid(minX: Int, maxX: Int, minY: Int, maxY: Int, minZ: Int, maxZ: Int) {
  override def toString: String = f"x:($minX, $maxX), y:($minY, $maxY), z:($minZ, $maxZ)"

  def size: BigInt =
    BigInt(1) * (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)

  def subtract(other: Cuboid): Seq[Cuboid] =
    Seq(
      Cuboid(minX, other.minX - 1, minY, other.minY - 1, minZ, other.minZ - 1),
      Cuboid(other.minX, other.maxX, minY, other.minY - 1, minZ, other.minZ - 1),
      Cuboid(other.maxX + 1, maxX, minY, other.minY - 1, minZ, other.minZ - 1),
      Cuboid(minX, other.minX - 1, other.minY, other.maxY, minZ, other.minZ - 1),
      Cuboid(other.minX, other.maxX, other.minY, other.maxY, minZ, other.minZ - 1),
      Cuboid(other.maxX + 1, maxX, other.minY, other.maxY, minZ, other.minZ - 1),
      Cuboid(minX, other.minX - 1, other.maxY + 1, maxY, minZ, other.minZ - 1),
      Cuboid(other.minX, other.maxX, other.maxY + 1, maxY, minZ, other.minZ - 1),
      Cuboid(other.maxX + 1, maxX, other.maxY + 1, maxY, minZ, other.minZ - 1),
      Cuboid(minX, other.minX - 1, minY, other.minY - 1, other.minZ, other.maxZ),
      Cuboid(other.minX, other.maxX, minY, other.minY - 1, other.minZ, other.maxZ),
      Cuboid(other.maxX + 1, maxX, minY, other.minY - 1, other.minZ, other.maxZ),
      Cuboid(minX, other.minX - 1, other.minY, other.maxY, other.minZ, other.maxZ),
      Cuboid(other.maxX + 1, maxX, other.minY, other.maxY, other.minZ, other.maxZ),
      Cuboid(minX, other.minX - 1, other.maxY + 1, maxY, other.minZ, other.maxZ),
      Cuboid(other.minX, other.maxX, other.maxY + 1, maxY, other.minZ, other.maxZ),
      Cuboid(other.maxX + 1, maxX, other.maxY + 1, maxY, other.minZ, other.maxZ),
      Cuboid(minX, other.minX - 1, minY, other.minY - 1, other.maxZ + 1, maxZ),
      Cuboid(other.minX, other.maxX, minY, other.minY - 1, other.maxZ + 1, maxZ),
      Cuboid(other.maxX + 1, maxX, minY, other.minY - 1, other.maxZ + 1, maxZ),
      Cuboid(minX, other.minX - 1, other.minY, other.maxY, other.maxZ + 1, maxZ),
      Cuboid(other.minX, other.maxX, other.minY, other.maxY, other.maxZ + 1, maxZ),
      Cuboid(other.maxX + 1, maxX, other.minY, other.maxY, other.maxZ + 1, maxZ),
      Cuboid(minX, other.minX - 1, other.maxY + 1, maxY, other.maxZ + 1, maxZ),
      Cuboid(other.minX, other.maxX, other.maxY + 1, maxY, other.maxZ + 1, maxZ),
      Cuboid(other.maxX + 1, maxX, other.maxY + 1, maxY, other.maxZ + 1, maxZ),
    )
      .map(_.cuboidInGrid(minX, maxX, minY, maxY, minZ, maxZ))
      .filterNot(_.emptyCuboid)

  def cuboidInGrid(
                    gridMinX: Int, gridMaxX: Int,
                    gridMinY: Int, gridMaxY: Int,
                    gridMinZ: Int, gridMaxZ: Int
                  ): Cuboid =
    Cuboid(
      math.max(minX, gridMinX),
      math.min(maxX, gridMaxX),
      math.max(minY, gridMinY),
      math.min(maxY, gridMaxY),
      math.max(minZ, gridMinZ),
      math.min(maxZ, gridMaxZ)
    )

  def emptyCuboid: Boolean = minX > maxX || minY > maxY || minZ > maxZ
}

case class RebootStep(on: Boolean, cuboid: Cuboid) {
  override def toString: String = f"Turning ${if (on) "on" else "off"} the cuboid $cuboid"

  def filterOutOfGrid(
                       gridMinX: Int, gridMaxX: Int,
                       gridMinY: Int, gridMaxY: Int,
                       gridMinZ: Int, gridMaxZ: Int
                     ): RebootStep =
    this.copy(cuboid = cuboid.cuboidInGrid(gridMinX, gridMaxX, gridMinY, gridMaxY, gridMinZ, gridMaxZ))

  def combine(cuboids: Seq[Cuboid]): Seq[Cuboid] =
    if (on)
      cuboids ++ cuboids.foldLeft(Seq(cuboid))((piecesOfCuboids, c) => {
        piecesOfCuboids.flatMap(_.subtract(c))
      })
    else
      cuboids.foldLeft(Seq.empty[Cuboid])((remainingPiecesOfCuboids, c) => {
        remainingPiecesOfCuboids ++ c.subtract(cuboid)
      })
}

object Reactor {
  def parseRebootStep(representation: String): RebootStep = {
    val step = raw"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)".r
    representation match {
      case step(onOrOff, fromX, toX, fromY, toY, fromZ, toZ) =>
        val on = onOrOff.equals("on")
        val (minX, maxX) = if (fromX.toInt < toX.toInt) (fromX.toInt, toX.toInt) else (toX.toInt, fromX.toInt)
        val (minY, maxY) = if (fromY.toInt < toY.toInt) (fromY.toInt, toY.toInt) else (toY.toInt, fromY.toInt)
        val (minZ, maxZ) = if (fromZ.toInt < toZ.toInt) (fromZ.toInt, toZ.toInt) else (toZ.toInt, fromZ.toInt)
        RebootStep(on, Cuboid(minX, maxX, minY, maxY, minZ, maxZ))
    }
  }

  @tailrec
  def executeSteps(rebootSteps: Seq[RebootStep], currentlyOn: BigInt = 0): BigInt =
    if (rebootSteps.isEmpty) currentlyOn
    else {
      val rebootStep = rebootSteps.head
      Console.println(f"Currently, $currentlyOn cubes are on. $rebootStep.")
      if (rebootStep.on) {
        val onOnlyThisStepAndNeverTurnedOff = rebootSteps.tail.foldLeft(Seq(rebootStep.cuboid))((remainingCuboids, step) => {
          remainingCuboids.flatMap(c => c.subtract(step.cuboid))
        })
        executeSteps(rebootSteps.tail, currentlyOn + onOnlyThisStepAndNeverTurnedOff.map(_.size).sum)
      }
      else executeSteps(rebootSteps.tail, currentlyOn)
    }
}
