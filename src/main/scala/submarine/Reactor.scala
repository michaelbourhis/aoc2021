package submarine

import scala.annotation.tailrec

case class Cuboid(minX: Int, maxX: Int, minY: Int, maxY: Int, minZ: Int, maxZ: Int) {
  override def toString: String = f"x:($minX, $maxX), y:($minY, $maxY), z:($minZ, $maxZ)"

  def size: BigInt =
    BigInt(1) * (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)

  def subtract(other: Cuboid): Seq[Cuboid] =
    if (other.maxX < minX || other.minX > maxX || other.maxY < minY || other.minY > maxY || other.maxZ < minZ || other.minZ > maxZ) Seq(this)
    else {
      val cutOnX = Seq(this.copy(maxX = other.minX - 1), this.copy(minX = other.maxX + 1))
      val xSlice = this.copy(minX = other.minX, maxX = other.maxX)
      val cutOnY = Seq(xSlice.copy(maxY = other.minY - 1), xSlice.copy(minY = other.maxY + 1))
      val xySlice = xSlice.copy(minY = other.minY, maxY = other.maxY)
      val cutOnZ = Seq(xySlice.copy(maxZ = other.minZ - 1), xySlice.copy(minZ = other.maxZ + 1))
      (cutOnX ++ cutOnY ++ cutOnZ)
        .map(fitsMySelf)
        .filterNot(_.emptyCuboid)
    }

  def fitsMySelf(cuboid: Cuboid): Cuboid =
    cuboid.cuboidInGrid(minX, maxX, minY, maxY, minZ, maxZ)

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
