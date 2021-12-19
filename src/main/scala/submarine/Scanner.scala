package submarine

import scala.annotation.tailrec

sealed trait Rotation

case object NoRotation extends Rotation

case object RotateOne extends Rotation

case object RotateTwo extends Rotation

case object RotateThree extends Rotation

case object Negative extends Rotation

case object NegativeRotateOne extends Rotation

case object NegativeRotateTwo extends Rotation

case object NegativeRotateThree extends Rotation

case object FaceUpY extends Rotation

case object FaceUpYRotateOne extends Rotation

case object FaceUpYRotateTwo extends Rotation

case object FaceUpYRotateThree extends Rotation

case object FaceUpYNegative extends Rotation

case object FaceUpYNegativeRotateOne extends Rotation

case object FaceUpYNegativeRotateTwo extends Rotation

case object FaceUpYNegativeRotateThree extends Rotation

case object FaceUpZ extends Rotation

case object FaceUpZRotateOne extends Rotation

case object FaceUpZRotateTwo extends Rotation

case object FaceUpZRotateThree extends Rotation

case object FaceUpZNegative extends Rotation

case object FaceUpZNegativeRotateOne extends Rotation

case object FaceUpZNegativeRotateTwo extends Rotation

case object FaceUpZNegativeRotateThree extends Rotation

case class Beacon(x: Int, y: Int, z: Int) {
  def diff(b: Beacon): (Int, Int, Int) = (x - b.x, y - b.y, z - b.z)

  def changeOrigin(origin: (Int, Int, Int)): Beacon = Beacon(x + origin._1, y + origin._2, z + origin._3)

  def negative: Beacon = this.copy(x = -x, y = -y)

  def rotate: Beacon = this.copy(y = -z, z = y)

  def faceUpY: Beacon = Beacon(y, z, x)

  def faceUpZ: Beacon = Beacon(z, x, y)

  def rotate(rotation: Rotation): Beacon = rotation match {
    case NoRotation => this
    case RotateOne => this.rotate
    case RotateTwo => this.rotate.rotate
    case RotateThree => this.rotate.rotate.rotate
    case Negative => this.negative
    case NegativeRotateOne => this.negative.rotate
    case NegativeRotateTwo => this.negative.rotate.rotate
    case NegativeRotateThree => this.negative.rotate.rotate.rotate
    case FaceUpY => this.faceUpY
    case FaceUpYRotateOne => this.faceUpY.rotate
    case FaceUpYRotateTwo => this.faceUpY.rotate.rotate
    case FaceUpYRotateThree => this.faceUpY.rotate.rotate.rotate
    case FaceUpYNegative => this.faceUpY.negative
    case FaceUpYNegativeRotateOne => this.faceUpY.negative.rotate
    case FaceUpYNegativeRotateTwo => this.faceUpY.negative.rotate.rotate
    case FaceUpYNegativeRotateThree => this.faceUpY.negative.rotate.rotate.rotate
    case FaceUpZ => this.faceUpZ
    case FaceUpZRotateOne => this.faceUpZ.rotate
    case FaceUpZRotateTwo => this.faceUpZ.rotate.rotate
    case FaceUpZRotateThree => this.faceUpZ.rotate.rotate.rotate
    case FaceUpZNegative => this.faceUpZ.negative
    case FaceUpZNegativeRotateOne => this.faceUpZ.negative.rotate
    case FaceUpZNegativeRotateTwo => this.faceUpZ.negative.rotate.rotate
    case FaceUpZNegativeRotateThree => this.faceUpZ.negative.rotate.rotate.rotate
  }
}

case class Scanner(id: Int, beacons: Seq[Beacon], origin: (Int, Int, Int) = (-1, -1, -1)) {
  def rotations(): Seq[(Rotation, Seq[Beacon])] =
    Seq(
      NoRotation,
      RotateOne,
      RotateTwo,
      RotateThree,
      Negative,
      NegativeRotateOne,
      NegativeRotateTwo,
      NegativeRotateThree,
      FaceUpY,
      FaceUpYRotateOne,
      FaceUpYRotateTwo,
      FaceUpYRotateThree,
      FaceUpYNegative,
      FaceUpYNegativeRotateOne,
      FaceUpYNegativeRotateTwo,
      FaceUpYNegativeRotateThree,
      FaceUpZ,
      FaceUpZRotateOne,
      FaceUpZRotateTwo,
      FaceUpZRotateThree,
      FaceUpZNegative,
      FaceUpZNegativeRotateOne,
      FaceUpZNegativeRotateTwo,
      FaceUpZNegativeRotateThree
    ).map(r => (r, beacons.map(_.rotate(r))))
}

object Scanner {
  def parseScanner(lines: Iterator[String]): Seq[Scanner] = {
    val scannerInitLine = raw"--- scanner (\d+) ---".r
    if (lines.nonEmpty) {
      val id = lines.next() match {
        case scannerInitLine(idAsString) => idAsString.toInt
      }
      val beacons = parseBeacons(lines)
      Seq(Scanner(id, beacons)) ++ parseScanner(lines)
    } else Seq.empty
  }

  def parseBeacons(lines: Iterator[String]): Seq[Beacon] = {
    if (lines.hasNext) {
      val line = lines.next()
      if (line.isEmpty) Seq.empty
      else {
        val coordinates = line.split(",").map(_.toInt)
        Seq(Beacon(coordinates(0), coordinates(1), coordinates(2))) ++ parseBeacons(lines)
      }
    } else Seq.empty
  }

  @tailrec
  def matchAll(toMatch: Seq[Scanner], alreadyMatched: Seq[Scanner]): Seq[Scanner] = {
    if(toMatch.isEmpty) alreadyMatched
    else {
      val newMatches = toMatch.map(notMatched => {
        val matches = matchPair(Scanner(0, alreadyMatched.flatMap(_.beacons.toSet.toSeq), (0,0,0)), notMatched)
        if (matches.nonEmpty) {
          val (rotation, origin) = matches.head
          Some(notMatched.copy(
            beacons = notMatched.beacons.map(_.rotate(rotation).changeOrigin(origin)),
            origin = origin
          ))
        } else None
      })
      val newAlreadyMatched = alreadyMatched ++ newMatches.filter(_.isDefined).map(_.get)
      matchAll(toMatch.filterNot(s => newAlreadyMatched.exists(_.id.equals(s.id))), newAlreadyMatched)
    }
  }

  def matchPair(first: Scanner, second: Scanner): Seq[(Rotation, (Int, Int, Int))] = {
    second.rotations().map {
      case (rotation, beacons) =>
        (
          rotation,
          beacons.flatMap(beacon =>
            first.beacons.map(b => b.diff(beacon))
          )
            .groupBy(identity)
            .view
            .mapValues(_.size)
            .find(_._2 >= 12)
        )
    }
      .filter(_._2.isDefined)
      .map(rotationAndOrigin => (rotationAndOrigin._1, rotationAndOrigin._2.get._1))
  }
}
