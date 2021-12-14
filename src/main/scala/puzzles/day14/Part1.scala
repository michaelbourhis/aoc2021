package puzzles.day14

import submarine.Polymerization

import scala.io.Source
import scala.util.Using

object Part1 extends App {
  def mostCommonMinusLeastCommonAfterSteps(steps: Int): Long =
    Using.resource(Source.fromResource("day14.txt")) {
      file =>
        val lines = file.getLines()
        val initialTemplate = lines.next().toCharArray.toSeq
        val initialPairsAndCount = Polymerization.getPairsAndCount(initialTemplate)
        lines.next()
        val pairInsertionRules = lines.map(Polymerization.parsePairInsertionRule).toMap
        val elementsOccurrences =
          (1 to steps)
            .foldLeft(initialPairsAndCount)((pairsAndCount, _) => Polymerization.insertPairs(pairsAndCount, pairInsertionRules))
            .groupBy(_._1._1)
            .view
            .mapValues(v => v.values.sum)
            .toMap
        val withLastChar = elementsOccurrences ++ Map((initialTemplate.last, 1L + elementsOccurrences.getOrElse(initialTemplate.last, 0L)))
        val occurrences = withLastChar
          .values
          .toSeq
          .sorted
        occurrences.last - occurrences.head
    }

  Console.println(f"After 10 steps, subtracting the number of least occurring elements from the number of most occurring elements gives: ${mostCommonMinusLeastCommonAfterSteps(10)}.")
}
