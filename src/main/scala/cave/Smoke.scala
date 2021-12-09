package cave

case class Smoke(height: Int, rowIndex: Int = 0, colIndex: Int = 0) {
  def riskLevel: Int = height + 1

  def basin(allRows: Seq[Seq[Smoke]]): Set[Smoke] =
    Set(this) ++
      topBasin(allRows) ++
      bottomBasin(allRows) ++
      leftBasin(allRows) ++
      rightBasin(allRows)

  def topBasin(allRows: Seq[Seq[Smoke]]): Set[Smoke] =
    if (
      rowIndex != 0 &&
        allRows(rowIndex - 1)(colIndex).height > height &&
        allRows(rowIndex - 1)(colIndex).height != 9
    )
      allRows(rowIndex - 1)(colIndex).basin(allRows)
    else
      Set.empty

  def bottomBasin(allRows: Seq[Seq[Smoke]]): Set[Smoke] =
    if (
      rowIndex != allRows.length - 1 &&
        allRows(rowIndex + 1)(colIndex).height > height &&
        allRows(rowIndex + 1)(colIndex).height != 9
    )
      allRows(rowIndex + 1)(colIndex).basin(allRows)
    else
      Set.empty

  def leftBasin(allRows: Seq[Seq[Smoke]]): Set[Smoke] =
    if (
      colIndex != 0 &&
        allRows(rowIndex)(colIndex - 1).height > height &&
        allRows(rowIndex)(colIndex - 1).height != 9
    )
      allRows(rowIndex)(colIndex - 1).basin(allRows)
    else
      Set.empty

  def rightBasin(allRows: Seq[Seq[Smoke]]): Set[Smoke] =
    if (
      colIndex != allRows.head.length - 1 &&
        allRows(rowIndex)(colIndex + 1).height > height &&
        allRows(rowIndex)(colIndex + 1).height != 9
    )
      allRows(rowIndex)(colIndex + 1).basin(allRows)
    else
      Set.empty

}

case class Status(
                   lowPoints: Seq[Smoke],
                   firstRow: Seq[Smoke] = Seq.empty,
                   secondRow: Seq[Smoke] = Seq.empty,
                   allRows: Seq[Seq[Smoke]] = Seq.empty,
                   currentRowIndex: Int = -1
                 )

object Smoke {
  def parse(representation: String): Seq[Smoke] =
    representation.toCharArray.map(c => Smoke(c.toString.toInt))

  def initStatus: Status =
    Status(Seq.empty)

  def findLowPoints(smokeLine: Seq[Smoke], previousSmokeLine: Seq[Smoke] = Seq.empty, nextSmokeLine: Seq[Smoke] = Seq.empty): Seq[Smoke] = {
    val lastIndex = smokeLine.length - 1
    smokeLine.zipWithIndex.filter {
      case (smoke, 0) => smoke.height < smokeLine(1).height &&
        (previousSmokeLine.isEmpty || smoke.height < previousSmokeLine.head.height) &&
        (nextSmokeLine.isEmpty || smoke.height < nextSmokeLine.head.height)
      case (smoke, i) if i == lastIndex => smoke.height < smokeLine(lastIndex - 1).height &&
        (previousSmokeLine.isEmpty || smoke.height < previousSmokeLine.last.height) &&
        (nextSmokeLine.isEmpty || smoke.height < nextSmokeLine.last.height)
      case (smoke, i) => smoke.height < smokeLine(i - 1).height && smoke.height < smokeLine(i + 1).height &&
        (previousSmokeLine.isEmpty || smoke.height < previousSmokeLine(i).height) &&
        (nextSmokeLine.isEmpty || smoke.height < nextSmokeLine(i).height)
    }.map(smokeAndIndex => smokeAndIndex._1)
  }
}
