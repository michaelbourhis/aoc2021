package cave

case class Cave(name: String) {
  def small: Boolean = name.toLowerCase().equals(name)

  def start: Boolean = name.equals("start")

  def end: Boolean = name.equals("end")
}

object Pathing {
  type MapOfTheCaves = Map[Cave, Set[Cave]]

  def parse(representation: Seq[String]): MapOfTheCaves =
    representation
      .map(_.split("-"))
      .flatMap {
        case Array(a, b) =>
          Seq((Cave(a), Set(Cave(b))), (Cave(b), Set(Cave(a))))
      }
      .groupBy(_._1)
      .view
      .mapValues(caveAndDestinations =>
        caveAndDestinations.flatMap(
          caveAndDestination => caveAndDestination._2
        ).toSet
      ).toMap

  def pathToEnd(currentPath: Seq[Cave], mapOfTheCaves: MapOfTheCaves, canVisitSmallTwice: Boolean = false): Seq[Seq[Cave]] = {
    val lastCaveVisited = currentPath.last
    if (lastCaveVisited.end)
      Seq(currentPath)
    else {
      val cannotVisitAnotherSmallCaveTwice = if (canVisitSmallTwice)
        currentPath
          .filter(_.small)
          .groupBy(identity)
          .view
          .mapValues(_.size)
          .exists(caveAndTimesVisited => caveAndTimesVisited._2 > 1)
      else
        true
      mapOfTheCaves(lastCaveVisited)
        .filterNot(cave => currentPath.contains(cave) && cave.small && (cave.start || cave.end || cannotVisitAnotherSmallCaveTwice))
        .map(cave => pathToEnd(currentPath ++ Seq(cave), mapOfTheCaves, canVisitSmallTwice = canVisitSmallTwice))
        .toSeq
        .flatten
    }
  }
}
