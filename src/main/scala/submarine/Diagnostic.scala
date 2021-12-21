package submarine

import scala.annotation.tailrec

object Diagnostic {
  type BitDistributions = Array[Int]
  type Bits = Array[Boolean]

  def getBits(bitsRepresentation: String): Bits =
    bitsRepresentation.map {
      case '0' => false
      case '1' => true
      case _ => throw new Exception(f"Unknown bit representation")
    }.toArray

  def initZerosAndOnes: (Array[Bits], Array[Bits]) = (Array(), Array())

  def initBitDistributions: BitDistributions = Array()

  def separateZerosAndOnes(position: Int)(zerosAndOnes: (Array[Bits], Array[Bits]), bits: Bits): (Array[Bits], Array[Bits]) =
    if (bits(position)) (zerosAndOnes._1, zerosAndOnes._2 ++ Array(bits))
    else (zerosAndOnes._1 ++ Array(bits), zerosAndOnes._2)

  def parse(bitDistributions: BitDistributions, newDiagnostic: Bits): BitDistributions = {
    if (bitDistributions.length == 0)
      newDiagnostic.map {
        case true => 1
        case false => -1
      }
    else
      newDiagnostic.zipWithIndex.map {
        case (true, index) => bitDistributions(index) + 1
        case (false, index) => bitDistributions(index) - 1
      }
  }

  def powerConsumption(bitDistributions: BitDistributions): Int = {
    val gammaRate = Integer.parseInt(bitDistributions.map(b => if (b > 0) "1" else "0").mkString, 2)
    val epsilonRate = Integer.parseInt(bitDistributions.map(b => if (b > 0) "0" else "1").mkString, 2)
    gammaRate * epsilonRate
  }

  def lifeSupportRating(beginsWithZero: Array[Bits], beginsWithOne: Array[Bits]): Int = {
    val (oxygenRatingInit, co2ScrubberRatingInit) =
      if (beginsWithZero.length > beginsWithOne.length) (beginsWithZero, beginsWithOne)
      else (beginsWithOne, beginsWithZero)
    Diagnostic.oxygenRating(oxygenRatingInit, 1) * Diagnostic.co2ScrubberRating(co2ScrubberRatingInit, 1)
  }

  @tailrec
  def oxygenRating(possibleBits: Array[Bits], position: Int): Int = {
    if (possibleBits.length == 1) bitsToInt(possibleBits(0))
    else {
      val (zeros, ones) = possibleBits.foldLeft(Diagnostic.initZerosAndOnes)(separateZerosAndOnes(position))
      if (zeros.length > ones.length) oxygenRating(zeros, position + 1)
      else oxygenRating(ones, position + 1)
    }
  }

  @tailrec
  def co2ScrubberRating(possibleBits: Array[Bits], position: Int): Int = {
    if (possibleBits.length == 1) bitsToInt(possibleBits(0))
    else {
      val (zeros, ones) = possibleBits.foldLeft(Diagnostic.initZerosAndOnes)(separateZerosAndOnes(position))
      if (ones.length < zeros.length) co2ScrubberRating(ones, position + 1)
      else co2ScrubberRating(zeros, position + 1)
    }
  }

  def bitsToInt(bits: Bits): Int = Integer.parseInt(bits.map(b => if (b) "1" else "0").mkString, 2)

}
