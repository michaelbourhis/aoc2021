package submarine

import scala.annotation.tailrec

abstract class Packet(val version: Int) {
  def totalVersion: Int
  def apply: Long
}

case class LiteralValue(override val version: Int, value: Long) extends Packet(version) {
  override def totalVersion: Int = version
  override def apply: Long = value
}

abstract class Operator(override val version: Int, val subPackets: Seq[Packet] = Seq.empty) extends Packet(version) {
  override def totalVersion: Int = version + subPackets.map(_.totalVersion).sum
}

case class Sum(override val version: Int, override val subPackets: Seq[Packet] = Seq.empty) extends Operator(version, subPackets) {
  override def apply: Long = subPackets.map(_.apply).sum
}

case class Product(override val version: Int, override val subPackets: Seq[Packet] = Seq.empty) extends Operator(version, subPackets) {
  override def apply: Long = subPackets.map(_.apply).product
}

case class Minimum(override val version: Int, override val subPackets: Seq[Packet] = Seq.empty) extends Operator(version, subPackets) {
  override def apply: Long = subPackets.map(_.apply).min
}

case class Maximum(override val version: Int, override val subPackets: Seq[Packet] = Seq.empty) extends Operator(version, subPackets) {
  override def apply: Long = subPackets.map(_.apply).max
}

case class GreaterThan(override val version: Int, override val subPackets: Seq[Packet] = Seq.empty) extends Operator(version, subPackets) {
  override def apply: Long = {
    val subPacketValues = subPackets.map(_.apply)
    if(subPacketValues.head > subPacketValues.last) 1L
    else 0L
  }
}

case class LessThan(override val version: Int, override val subPackets: Seq[Packet] = Seq.empty) extends Operator(version, subPackets) {
  override def apply: Long = {
    val subPacketValues = subPackets.map(_.apply)
    if(subPacketValues.head < subPacketValues.last) 1L
    else 0L
  }
}

case class EqualTo(override val version: Int, override val subPackets: Seq[Packet] = Seq.empty) extends Operator(version, subPackets) {
  override def apply: Long = {
    val subPacketValues = subPackets.map(_.apply)
    if(subPacketValues.head == subPacketValues.last) 1L
    else 0L
  }
}

object Bits {
  def parseTransmission(transmission: String): Seq[Boolean] =
    transmission.toCharArray.flatMap(hexToBits)

  @tailrec
  def readPackets(bits: Seq[Boolean], packets: Seq[Packet] = Seq.empty, maxPackets: Int = -1): (Seq[Packet], Seq[Boolean]) = {
    if (bits.length < 11)
      (packets, bits)
    else {
      val (version, bitsAfterVersion) = bits.splitAt(3)
      val (bitsForTypeId, bitsAfterTypeId) = bitsAfterVersion.splitAt(3)
      val typeId = bitsToInt(bitsForTypeId)
      val (packet, bitsAfterPacket) = typeId match {
        case 4 => getLiteralPacket(bitsAfterTypeId, bitsToInt(version))
        case _ => getOperatorPacket(typeId, bitsAfterTypeId, bitsToInt(version))
      }
      if (maxPackets == 1)
        (packets ++ Seq(packet), bitsAfterPacket)
      else if (maxPackets == -1)
        readPackets(bitsAfterPacket, packets ++ Seq(packet), maxPackets)
      else
        readPackets(bitsAfterPacket, packets ++ Seq(packet), maxPackets - 1)
    }
  }

  def getOperatorPacket(typeId: Int, bits: Seq[Boolean], version: Int): (Operator, Seq[Boolean]) = {
    val (lengthTypeId, bitsRemainingAfterLengthTypeId) = bits.splitAt(1)
    val (subPackets, bitsRemainingAfterSubPackets) = if (lengthTypeId.head) {
      val (numberOfPackets, bitsRemainingAfterNumberOfPackets) = bitsRemainingAfterLengthTypeId.splitAt(11)
      readPackets(bitsRemainingAfterNumberOfPackets, maxPackets = bitsToInt(numberOfPackets))
    } else {
      val (lengthOfPackets, bitsRemainingAfterLength) = bitsRemainingAfterLengthTypeId.splitAt(15)
      val (bitsForPackets, bitsAfterPackets) = bitsRemainingAfterLength.splitAt(bitsToInt(lengthOfPackets))
      (readPackets(bitsForPackets)._1, bitsAfterPackets)
    }
    val operatorClass = typeId match {
      case 0 => Sum
      case 1 => Product
      case 2 => Minimum
      case 3 => Maximum
      case 5 => GreaterThan
      case 6 => LessThan
      case 7 => EqualTo
    }
    (
      operatorClass(version, subPackets),
      bitsRemainingAfterSubPackets
    )
  }


  def getLiteralPacket(bits: Seq[Boolean], version: Int): (LiteralValue, Seq[Boolean]) = {
    val (digitBits, remainingBits) = readDigits(bits)
    (
      LiteralValue(version, bitsToLong(digitBits)),
      remainingBits
    )
  }

  def readDigits(bits: Seq[Boolean]): (Seq[Boolean], Seq[Boolean]) =
    if (bits.head) {
      val (nextDigits, remainingBits) = readDigits(bits.drop(5))
      (
        bits.slice(1, 5) ++ nextDigits,
        remainingBits
      )
    }
    else
      (
        bits.slice(1, 5),
        bits.drop(5)
      )

  def hexToBits(hex: Char): Seq[Boolean] = hex match {
    case '0' => Seq(false, false, false, false)
    case '1' => Seq(false, false, false, true)
    case '2' => Seq(false, false, true, false)
    case '3' => Seq(false, false, true, true)
    case '4' => Seq(false, true, false, false)
    case '5' => Seq(false, true, false, true)
    case '6' => Seq(false, true, true, false)
    case '7' => Seq(false, true, true, true)
    case '8' => Seq(true, false, false, false)
    case '9' => Seq(true, false, false, true)
    case 'A' => Seq(true, false, true, false)
    case 'B' => Seq(true, false, true, true)
    case 'C' => Seq(true, true, false, false)
    case 'D' => Seq(true, true, false, true)
    case 'E' => Seq(true, true, true, false)
    case 'F' => Seq(true, true, true, true)
  }

  def bitsToInt(bits: Seq[Boolean]): Int =
    bits.foldLeft(0)(
      (i, b) => (i << 1) + (if (b) 1 else 0)
    )

  def bitsToLong(bits: Seq[Boolean]): Long =
    bits.foldLeft(0L)(
      (i, b) => (i << 1) + (if (b) 1 else 0)
    )
}
