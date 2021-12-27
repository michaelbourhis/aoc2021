package animals

import scala.annotation.tailrec

trait SeaCucumber

case object EastSeaCucumber extends SeaCucumber

case object SouthSeaCucumber extends SeaCucumber

object SeaCucumber {
  type SeaFloor = Seq[Seq[Option[SeaCucumber]]]

  def parse(lines: Iterator[String]): SeaFloor =
    lines.map(line =>
      line.toCharArray.map {
        case '.' => None
        case '>' => Some(EastSeaCucumber)
        case 'v' => Some(SouthSeaCucumber)
      }.toSeq
    ).toSeq

  @tailrec
  def stepsUntilItStops(seaFloor: SeaFloor, stepNumber: Int = 1): Int = {
    val newFloor = makeAStep(seaFloor)
    if (newFloor.equals(seaFloor)) stepNumber
    else stepsUntilItStops(newFloor, stepNumber + 1)
  }

  def makeAStep(seaFloor: SeaFloor): SeaFloor =
    southMoves(eastMoves(seaFloor))

  def eastMoves(seaFloor: SeaFloor): SeaFloor =
    seaFloor.map(line => {
      line.zipWithIndex.map {
        case (None, i) =>
          val previous = if (i == 0) line.length - 1 else i - 1
          line(previous) match {
            case Some(EastSeaCucumber) => Some(EastSeaCucumber)
            case _ => None
          }
        case (Some(EastSeaCucumber), i) =>
          val next = if (i == line.length - 1) 0 else i + 1
          line(next) match {
            case None => None
            case _ => Some(EastSeaCucumber)
          }
        case (Some(SouthSeaCucumber), _) => Some(SouthSeaCucumber)
      }
    })

  def southMoves(seaFloor: SeaFloor): SeaFloor =
    seaFloor.zipWithIndex.map {
      case (line, i) =>
        line.zipWithIndex.map {
          case (None, j) =>
            val previous = if (i == 0) seaFloor.length - 1 else i - 1
            seaFloor(previous)(j) match {
              case Some(SouthSeaCucumber) => Some(SouthSeaCucumber)
              case _ => None
            }
          case (Some(SouthSeaCucumber), j) =>
            val next = if (i == seaFloor.length - 1) 0 else i + 1
            seaFloor(next)(j) match {
              case None => None
              case _ => Some(SouthSeaCucumber)
            }
          case (Some(EastSeaCucumber), _) => Some(EastSeaCucumber)
        }
    }
}
