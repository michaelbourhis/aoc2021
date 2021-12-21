package submarine

case class Digit(segments: Seq[Char]) {
  def includedIn(other: Digit): Boolean =
    segments.forall(c => other.segments.contains(c))
}

case class Note(patterns: Seq[Digit], output: Seq[Digit], digitMapping: Map[Digit, Int] = Map.empty)

object Display {
  val digitOneSize = 2
  val digitFourSize = 4
  val digitSevenSize = 3
  val digitEightSize = 7
  val digitTwoOrThreeOrFiveSize = 5
  val digitZeroOrSixOrNineSize = 6
  val obviousSizes = Seq(digitOneSize, digitFourSize, digitSevenSize, digitEightSize)

  def parse(representation: String): Note = {
    representation.split("\\|") match {
      case Array(patterns, output) =>
        Note(
          parseDigits(patterns),
          parseDigits(output)
        )
    }
  }

  def parseDigits(stringListOfDigits: String): Seq[Digit] =
    stringListOfDigits.trim.split(" ").map(toDigit)

  def toDigit(chars: String): Digit =
    Digit(chars.toCharArray.toSeq.sorted)

  def guessDigits(note: Note): Note = {
    val digitOne = note.patterns.find(_.segments.length == digitOneSize).get
    val digitFour = note.patterns.find(_.segments.length == digitFourSize).get
    val digitSeven = note.patterns.find(_.segments.length == digitSevenSize).get
    val digitEight = note.patterns.find(_.segments.length == digitEightSize).get
    val digitTwoOrThreeOrFive = note.patterns.filter(_.segments.length == digitTwoOrThreeOrFiveSize)
    val digitZeroOrSixOrNine = note.patterns.filter(_.segments.length == digitZeroOrSixOrNineSize)
    val digitThree = digitTwoOrThreeOrFive.find(digitSeven.includedIn).get
    val digitNine = digitZeroOrSixOrNine.find(digitThree.includedIn).get
    val digitZero = digitZeroOrSixOrNine.find(d => !d.equals(digitNine) && digitOne.includedIn(d)).get
    val digitSix = digitZeroOrSixOrNine.find(d => !d.equals(digitNine) && !d.equals(digitZero)).get
    val digitFive = digitTwoOrThreeOrFive.find(d => !d.equals(digitThree) && d.includedIn(digitNine)).get
    val digitTwo = digitTwoOrThreeOrFive.find(d => !d.equals(digitThree) && !d.equals(digitFive)).get
    note.copy(digitMapping = Map(
      digitOne -> 1,
      digitTwo -> 2,
      digitThree -> 3,
      digitFour -> 4,
      digitFive -> 5,
      digitSix -> 6,
      digitSeven -> 7,
      digitEight -> 8,
      digitNine -> 9,
      digitZero -> 0,
    ))
  }
}
