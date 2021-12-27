package submarine

import scala.annotation.tailrec

object ALU {
  def parse(lines: Iterator[String]): Seq[(Int, Int, Int)] =
    lines.grouped(18).zipWithIndex.map {
      case (instructions, position) =>
        (
          position,
          instructions(5).split(" ").last.toInt,
          instructions(15).split(" ").last.toInt
        )
    }.toSeq

  @tailrec
  def resolve(operations: Seq[(Int, Int, Int)], numbers: Seq[Int] = Seq.fill(14)(0), getSmallest: Boolean = false): Long =
    if (operations.isEmpty) numbers.mkString.toLong
    else {
      val firstRemoveIndex = operations.indexWhere(_._2 <= 0)
      val add = operations(firstRemoveIndex - 1)
      val remove = operations(firstRemoveIndex)
      val diffBetweenRemoveAndAdd = remove._2 + add._3
      val digitValue = if (getSmallest) 1 else 9
      val (addNumber, removeNumber) =
        if (diffBetweenRemoveAndAdd > 0 == getSmallest)
          (
            digitValue,
            digitValue + diffBetweenRemoveAndAdd,
          )
        else
          (
            digitValue - diffBetweenRemoveAndAdd,
            digitValue
          )
      resolve(
        operations.take(firstRemoveIndex - 1) ++ operations.drop(firstRemoveIndex + 1),
        numbers.updated(remove._1, removeNumber).updated(add._1, addNumber),
        getSmallest
      )
    }
}
