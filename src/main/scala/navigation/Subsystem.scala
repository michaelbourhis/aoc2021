package navigation

case class Line(incompleteChunk: Seq[Chunk] = Seq.empty, illegalCharacter: Option[Char] = None) {
  def score: Long = illegalCharacter match {
    case None => 0
    case Some(')') => 3
    case Some(']') => 57
    case Some('}') => 1197
    case Some('>') => 25137
  }

  def illegal: Boolean = illegalCharacter.isDefined

  def completeLine: Long =
    incompleteChunk.foldRight(0L)((chunk, score) => {
      score * 5L + (chunk match {
        case Chunk('(') => 1L
        case Chunk('[') => 2L
        case Chunk('{') => 3L
        case Chunk('<') => 4L
      })
    })
}

case class Chunk(openingSymbol: Char)

object Subsystem {
  def openingChar(char: Char): Boolean =
    Seq('(', '[', '{', '<').contains(char)

  def correspondingOpeningChunk(char: Char): Chunk = char match {
    case ')' => Chunk('(')
    case ']' => Chunk('[')
    case '}' => Chunk('{')
    case '>' => Chunk('<')
  }

  def parse(representation: String): Line =
    representation
      .toCharArray
      .foldLeft(Line())((line, newChar) =>
        if (line.illegal)
          line
        else if (openingChar(newChar))
          line.copy(incompleteChunk = line.incompleteChunk ++ Seq(Chunk(newChar)))
        else {
          val chunkClosedByThisChar = correspondingOpeningChunk(newChar)
          if (line.incompleteChunk.nonEmpty && line.incompleteChunk.last.equals(chunkClosedByThisChar))
            line.copy(incompleteChunk = line.incompleteChunk.dropRight(1))
          else
            line.copy(illegalCharacter = Some(newChar))
        }
      )
}
