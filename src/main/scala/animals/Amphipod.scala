package animals

import scala.annotation.tailrec
import scala.collection.SortedSet

trait AmphipodType {
  def energy: Int
}

object NoOne extends AmphipodType {
  override def toString: String = "."

  override def energy: Int = 0
}

trait SomeOne extends AmphipodType

object Amber extends SomeOne {
  override def toString: String = "A"

  override def energy: Int = 1
}

object Bronze extends SomeOne {
  override def toString: String = "B"

  override def energy: Int = 10
}

object Copper extends SomeOne {
  override def toString: String = "C"

  override def energy: Int = 100
}

object Desert extends SomeOne {
  override def toString: String = "D"

  override def energy: Int = 1000
}

case class Burrow(
                   hallway: Seq[AmphipodType],
                   amberRoom: Seq[AmphipodType],
                   bronzeRoom: Seq[AmphipodType],
                   copperRoom: Seq[AmphipodType],
                   desertRoom: Seq[AmphipodType],
                   roomEntries: Seq[Int]
                 ) {
  override def toString: String =
    f"In hallway: ${hallway.map(_.toString).mkString} - In amber: ${amberRoom.map(_.toString).mkString} - In bronze: ${bronzeRoom.map(_.toString).mkString} - In copper: ${copperRoom.map(_.toString).mkString} - In desert: ${desertRoom.map(_.toString).mkString}"

  def impossibleHallwayState: Boolean =
    hallway
      .zipWithIndex
      .filterNot {
        case (NoOne, _) => true
        case (_, position) =>
          roomEntries.contains(position) && position > 1 && position < hallway.size - 2
      }
      .exists {
        case (someOne, position) =>
          val sideRoomPosition = roomEntryPosition(someOne)
          val (start, end, reverse) = if (sideRoomPosition < position) (sideRoomPosition + 1, position, true) else (position + 1, sideRoomPosition, false)
          hallway
            .slice(start, end)
            .filterNot(_.equals(NoOne))
            .filterNot(_.equals(someOne))
            .exists(otherOne => {
              if (reverse) roomEntryPosition(otherOne) > position
              else roomEntryPosition(otherOne) < position
            })
      }

  @tailrec
  final def putInFinalPosition(energyConsumedByPreviousMoves: Int = 0): (Burrow, Int) = {
    val (burrowAfterHallwayMoves, energyAfterHallwayMoves) = hallwayMoves()
    val (burrowAfterSideToSideMoves, energyAfterSideToSideMoves) = burrowAfterHallwayMoves.sideToSideMoves()
    if (energyAfterSideToSideMoves == 0) (burrowAfterSideToSideMoves, energyConsumedByPreviousMoves + energyAfterHallwayMoves + energyAfterSideToSideMoves)
    else burrowAfterSideToSideMoves.putInFinalPosition(energyConsumedByPreviousMoves + energyAfterHallwayMoves + energyAfterSideToSideMoves)
  }

  @tailrec
  final def sideToSideMoves(energyConsumedByPreviousMoves: Int = 0): (Burrow, Int) =
    Seq(Amber, Bronze, Copper, Desert).find(roomType => {
      if (someOneCanBeMovedFromSideRoom(roomType)) {
        val positionInSideRoomOfMovingAmphipod = firstOccupiedPositionInSideRoom(getSideRoom(roomType))
        val movingAmphipod = getSideRoom(roomType)(positionInSideRoomOfMovingAmphipod)
        !movingAmphipod.equals(roomType) && canEnterSideRoom(movingAmphipod) && canPathToSideRoom(movingAmphipod, roomEntryPosition(roomType))
      }
      else
        false
    })
      .map(originRoom => {
        val positionInsideRoomOfMovingAmphipod = firstOccupiedPositionInSideRoom(getSideRoom(originRoom))
        val movingAmphipod = getSideRoom(originRoom)(positionInsideRoomOfMovingAmphipod)
        val position = roomEntryPosition(originRoom)
        val (burrowAfterSideToHallwayMove, energyAfterSideToHallwayMove) = moveFromSideRoomToHallway(originRoom, positionInsideRoomOfMovingAmphipod, movingAmphipod, position)
        val (burrowAfterHallwayToSideMove, energyAfterHallwayToSideMove) = burrowAfterSideToHallwayMove.moveFromHallwayToItsSideRoom(position)
        (burrowAfterHallwayToSideMove, energyAfterSideToHallwayMove + energyAfterHallwayToSideMove)
      }) match {
      case None => (this, energyConsumedByPreviousMoves)
      case Some((burrow, energyConsumed)) => burrow.sideToSideMoves(energyConsumedByPreviousMoves + energyConsumed)
    }

  @tailrec
  final def hallwayMoves(energyConsumedByPreviousMoves: Int = 0): (Burrow, Int) =
    hallway
      .zipWithIndex
      .filterNot(_._1.equals(NoOne))
      .find {
        case (someOne: SomeOne, position) => canEnterSideRoom(someOne) && canPathToSideRoom(someOne, position)
      }
      .map {
        case (_, position) => moveFromHallwayToItsSideRoom(position)
      } match {
      case None => (this, energyConsumedByPreviousMoves)
      case Some((burrow, energyConsumed)) => burrow.hallwayMoves(energyConsumedByPreviousMoves + energyConsumed)
    }

  def sideRoomMoves: Seq[(Burrow, Int)] =
    Seq(Amber, Bronze, Copper, Desert).flatMap(roomType =>
      if (someOneCanBeMovedFromSideRoom(roomType)) {
        val positionInsideRoomOfMovingAmphipod = firstOccupiedPositionInSideRoom(getSideRoom(roomType))
        val movingAmphipod = getSideRoom(roomType)(positionInsideRoomOfMovingAmphipod)
        val leftWay = hallway.take(roomEntryPosition(roomType))
        val leftPositions = (leftWay.lastIndexWhere(!_.equals(NoOne)) + 1 until roomEntryPosition(roomType)).filterNot(roomEntries.contains)
        val lastRightPosition = hallway.indexWhere(!_.equals(NoOne), roomEntryPosition(roomType) + 1) match {
          case -1 => hallway.size
          case i => i
        }
        val rightPositions = (roomEntryPosition(roomType) + 1 until lastRightPosition).filterNot(roomEntries.contains)
        (leftPositions ++ rightPositions).map(position => {
          moveFromSideRoomToHallway(roomType, positionInsideRoomOfMovingAmphipod, movingAmphipod, position)
        })
      } else
        Seq.empty
    )

  def moveFromSideRoomToHallway(
                                 sideRoomType: SomeOne,
                                 positionInSideRoomOfMovingAmphipod: Int,
                                 movingAmphipod: AmphipodType,
                                 positionInHallway: Int
                               ): (Burrow, Int) =
    (
      this.copy(hallway = hallway.updated(positionInHallway, movingAmphipod)).moveOutOfSideRoom(sideRoomType),
      energyConsumedAfterTraveling(
        movingOne = movingAmphipod,
        positionInHallWay = positionInHallway,
        positionInTargetSideRoom = positionInSideRoomOfMovingAmphipod,
        targetSideRoomType = sideRoomType
      )
    )

  def someOneCanBeMovedFromSideRoom(someOne: AmphipodType): Boolean =
    getSideRoom(someOne).exists(a => !a.equals(someOne) && !a.equals(NoOne))

  def canEnterSideRoom(someOne: AmphipodType): Boolean =
    getSideRoom(someOne).filterNot(_.equals(NoOne)).forall(_.equals(someOne))

  def canPathToSideRoom(someOne: AmphipodType, position: Int): Boolean = {
    val entryPosition = roomEntryPosition(someOne)
    val (start, end) = if (entryPosition < position) (entryPosition + 1, position) else (position + 1, entryPosition)
    hallway.slice(start, end).forall(_.equals(NoOne))
  }

  def moveFromHallwayToItsSideRoom(position: Int): (Burrow, Int) =
    (
      this.copy(hallway = hallway.updated(position, NoOne)).newComerInSideRoom(hallway(position)),
      energyConsumedAfterTraveling(
        movingOne = hallway(position),
        positionInHallWay = position,
        positionInTargetSideRoom = lastEmptyPositionInSideRoom(getSideRoom(hallway(position))),
        targetSideRoomType = hallway(position)
      )
    )

  def newComerInSideRoom(someOne: AmphipodType): Burrow = {
    val updatedSideRoom = getSideRoom(someOne).updated(lastEmptyPositionInSideRoom(getSideRoom(someOne)), someOne)
    someOne match {
      case Amber => this.copy(amberRoom = updatedSideRoom)
      case Bronze => this.copy(bronzeRoom = updatedSideRoom)
      case Copper => this.copy(copperRoom = updatedSideRoom)
      case Desert => this.copy(desertRoom = updatedSideRoom)
    }
  }

  def moveOutOfSideRoom(someOne: AmphipodType): Burrow = {
    val position = firstOccupiedPositionInSideRoom(getSideRoom(someOne))
    val updatedSideRoom = getSideRoom(someOne).updated(position, NoOne)
    someOne match {
      case Amber => this.copy(amberRoom = updatedSideRoom)
      case Bronze => this.copy(bronzeRoom = updatedSideRoom)
      case Copper => this.copy(copperRoom = updatedSideRoom)
      case Desert => this.copy(desertRoom = updatedSideRoom)
    }
  }

  def lastEmptyPositionInSideRoom(sideRoom: Seq[AmphipodType]): Int = sideRoom.lastIndexOf(NoOne)

  def firstOccupiedPositionInSideRoom(sideRoom: Seq[AmphipodType]): Int = sideRoom.lastIndexOf(NoOne) + 1

  def getSideRoom(someOne: AmphipodType): Seq[AmphipodType] = someOne match {
    case Amber => amberRoom
    case Bronze => bronzeRoom
    case Copper => copperRoom
    case Desert => desertRoom
  }

  def energyConsumedAfterTraveling(
                                    movingOne: AmphipodType,
                                    positionInHallWay: Int,
                                    positionInTargetSideRoom: Int,
                                    targetSideRoomType: AmphipodType
                                  ): Int =
    (
      math.abs(positionInHallWay - roomEntryPosition(targetSideRoomType)) +
        positionInTargetSideRoom + 1
      ) * movingOne.energy

  def done: Boolean =
    amberRoom.forall(_.equals(Amber)) &&
      bronzeRoom.forall(_.equals(Bronze)) &&
      copperRoom.forall(_.equals(Copper)) &&
      desertRoom.forall(_.equals(Desert))

  def roomEntryPosition(someOne: AmphipodType): Int = someOne match {
    case Amber => roomEntries.head
    case Bronze => roomEntries(1)
    case Copper => roomEntries(2)
    case Desert => roomEntries.last
  }

}

case class Reorganization(
                           burrow: Burrow,
                           energyConsumed: Int = 0
                         ) extends Ordered[Reorganization] {
  override def toString: String =
    f"$burrow - Energy: $energyConsumed"

  def moves: Seq[Reorganization] =
    burrow.sideRoomMoves.filterNot {
      case (b, _) => b.impossibleHallwayState
    }.map {
      case (b, e) => b.putInFinalPosition(e)
    }.map {
      case (b, e) => Reorganization(
        burrow = b,
        energyConsumed = energyConsumed + e
      )
    }

  override def compare(that: Reorganization): Int = {
    if (this.equals(that)) 0
    else if (this.energyConsumed < that.energyConsumed) -1
    else 1
  }
}

object Amphipod {
  val unfoldedLines = Seq("DCBA", "DBAC")

  def parseStartingConfiguration(representation: Iterator[String], unfold: Boolean = false): Reorganization = {
    representation.next() // Top wall
    val hallwayLine = representation.next()
    val hallwaySize = hallwayLine.count(_.equals('.'))
    val hallwayBeginningIndex = hallwayLine.indexOf('.')
    val roomEntryLine = representation.next()
    val roomEntries = roomEntryLine.zipWithIndex.filterNot(_._1.equals('#')).map(_._2 - hallwayBeginningIndex)
    val rooms = (
      Seq(roomEntryLine) ++
        (if (unfold) unfoldedLines else Seq.empty) ++
        representation.toSeq
      ).map(_.trim.filterNot(_.equals('#'))).filterNot(_.isEmpty)
    val hallway = Seq.fill(hallwaySize)(NoOne)
    val amberRoom: Seq[AmphipodType] = rooms.map(l => letterToAmphipod(l(0)))
    val bronzeRoom: Seq[AmphipodType] = rooms.map(l => letterToAmphipod(l(1)))
    val copperRoom: Seq[AmphipodType] = rooms.map(l => letterToAmphipod(l(2)))
    val desertRoom: Seq[AmphipodType] = rooms.map(l => letterToAmphipod(l(3)))
    Reorganization(Burrow(hallway, amberRoom, bronzeRoom, copperRoom, desertRoom, roomEntries))
  }

  @tailrec
  def organise(burrows: SortedSet[Reorganization], knownOrganizations: Map[Burrow, Int]): Int = {
    val leastEnergyUsed = burrows.head
    if (leastEnergyUsed.burrow.done) leastEnergyUsed.energyConsumed
    else {
      val newReorganizations = leastEnergyUsed.moves
      val reorganizationsThatDoBetter = newReorganizations.filter(burrow =>
        knownOrganizations.get(burrow.burrow).forall(e => burrow.energyConsumed < e))
      val knownOrganizationsUpdated = knownOrganizations ++
        reorganizationsThatDoBetter.map(burrow => (burrow.burrow, burrow.energyConsumed)).toMap
      val updatedRemainingBurrows = burrows.tail.filter(
        burrow => reorganizationsThatDoBetter.forall(!_.burrow.equals(burrow.burrow))
      ) ++ reorganizationsThatDoBetter
      organise(updatedRemainingBurrows, knownOrganizationsUpdated)
    }
  }

  def letterToAmphipod(letter: Char): AmphipodType = letter match {
    case 'A' => Amber
    case 'B' => Bronze
    case 'C' => Copper
    case 'D' => Desert
    case '.' => NoOne
  }

}