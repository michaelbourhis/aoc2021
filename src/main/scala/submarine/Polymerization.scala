package submarine

object Polymerization {
  type Pair = (Char, Char)
  type PairInsertionRule = (Pair, Char)
  type PairInsertionRules = Map[Pair, Char]
  type MapOfPairsAndCount = Map[Pair, Long]

  def parsePairInsertionRule(representation: String): PairInsertionRule = {
    representation.split(" -> ") match {
      case Array(pair, insertedElement) => ((pair.head, pair.last), insertedElement.head)
    }
  }

  def getPairsAndCount(template: Seq[Char]): MapOfPairsAndCount =
    (0 to template.length - 2).map(i =>
      (template(i), template(i + 1))
    ).groupBy(identity)
      .view
      .mapValues(_.length.toLong)
      .toMap

  def insertPairs(pairsAndCount: MapOfPairsAndCount, pairInsertionRules: PairInsertionRules): MapOfPairsAndCount =
    pairsAndCount.foldLeft(Map.empty[Pair, Long])((newPairsAndCount, pairAndCount) => {
      val (pair, count) = pairAndCount
      if (pairInsertionRules.contains(pair)) {
        val newPair1 = (pair._1, pairInsertionRules(pair))
        val newPair2 = (pairInsertionRules(pair), pair._2)
        newPairsAndCount ++ Map(
          (newPair1, newPairsAndCount.getOrElse(newPair1, 0L) + count),
          (newPair2, newPairsAndCount.getOrElse(newPair2, 0L) + count),
        )
      } else
        newPairsAndCount ++ Map((pair, newPairsAndCount.getOrElse(pair, 0L) + count))
    })
}
