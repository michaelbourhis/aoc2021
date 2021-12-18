package animals

import scala.annotation.tailrec

sealed trait ActionDone

case object Nothing extends ActionDone

case class Explodes(left: Int, right: Int) extends ActionDone

case class PropagateLeft(added: Int) extends ActionDone

case class PropagateRight(added: Int) extends ActionDone

case object ExplodedPropagated extends ActionDone

case object Splits extends ActionDone

abstract class Element() {
  def reducePairs(level: Int): (Element, ActionDone)

  def reduceElements(): (Element, ActionDone)

  def propagateLeft(added: Int): Element

  def propagateRight(added: Int): Element

  def magnitude(): Int
}

case class Pair(left: Element, right: Element) extends Element() {
  override def toString: String = f"[$left,$right]"

  override def reducePairs(level: Int): (Element, ActionDone) = {
    if (level > 4) {
      (left, right) match {
        case (Number(l), Number(r)) => (Number(0), Explodes(l, r))
        case _ => throw new NotImplementedError(f"Unhandled case, pairs too deep: $this")
      }
    } else {
      val (leftReduced, leftAction) = left.reducePairs(level + 1)
      leftAction match {
        case Nothing => {
          val (rightReduced, rightAction) = right.reducePairs(level + 1)
          rightAction match {
            case Nothing => (this, Nothing)
            case Explodes(l, r) => (Pair(leftReduced.propagateRight(l), rightReduced), PropagateRight(r))
            case PropagateLeft(l) => (Pair(leftReduced.propagateRight(l), rightReduced), ExplodedPropagated)
            case actionDone: ActionDone => (this.copy(right = rightReduced), actionDone)
          }
        }
        case Explodes(l, r) => (Pair(left = leftReduced, right = right.propagateLeft(r)), PropagateLeft(l))
        case PropagateRight(r) => (Pair(left = leftReduced, right.propagateLeft(r)), ExplodedPropagated)
        case actionDone: ActionDone => (this.copy(left = leftReduced), actionDone)
      }
    }
  }

  override def reduceElements(): (Element, ActionDone) = {
    val (leftReduced, leftAction) = left.reduceElements()
    leftAction match {
      case Nothing => {
        val (rightReduced, rightAction) = right.reduceElements()
        rightAction match {
          case Nothing => (this, Nothing)
          case Splits => (this.copy(right = rightReduced), Splits)
        }
      }
      case Splits => (this.copy(left = leftReduced), Splits)
    }
  }

  override def propagateLeft(added: Int): Element = this.copy(left = left.propagateLeft(added))

  override def propagateRight(added: Int): Element = this.copy(right = right.propagateRight(added))

  override def magnitude(): Int = 3 * left.magnitude() + 2 * right.magnitude()
}

case class Number(value: Int) extends Element() {
  override def toString: String = f"$value"

  override def reduceElements(): (Element, ActionDone) =
    if (value > 9) (Pair(Number(value / 2), Number((value + 1) / 2)), Splits)
    else (this, Nothing)

  override def reducePairs(level: Int): (Element, ActionDone) = (this, Nothing)

  override def propagateLeft(added: Int): Element = propagate(added)

  override def propagateRight(added: Int): Element = propagate(added)

  def propagate(added: Int): Element = this.copy(value = value + added)

  override def magnitude(): Int = value
}


object Snailfish {
  def parseProblem(representation: String): Pair = {
    val (leftElement, rightElement, _) = parsePairElements(representation.drop(1))
    Pair(leftElement, rightElement)
  }

  def parsePairElements(str: String): (Element, Element, String) = {
    val numberElement = raw"(\d+)(.*)".r
    val (left, remainingAfterLeft) = str match {
      case numberElement(value, restOfString) => (Number(value.toInt), restOfString)
      case _ => parsePair(str)
    }
    val remainingAfterComma = remainingAfterLeft.drop(1)
    val (right, remainingAfterRight) = remainingAfterComma match {
      case numberElement(value, restOfString) => (Number(value.toInt), restOfString)
      case _ => parsePair(remainingAfterComma)
    }
    val remainingAfterPairFinish = remainingAfterRight.drop(1)
    (left, right, remainingAfterPairFinish)
  }


  def parsePair(str: String): (Element, String) = {
    val (leftElement, rightElement, remainingAfterPair) = parsePairElements(str.drop(1))
    (Pair(leftElement, rightElement), remainingAfterPair)
  }

  @tailrec
  def reduceIt(pair: Pair): Pair = {
    pair.reducePairs(1) match {
      case (p: Pair, Nothing) => {
        p.reduceElements() match {
          case (pFinal: Pair, Nothing) => pFinal
          case (pFinal: Pair, _: ActionDone) => reduceIt(pFinal)
          case _ => throw new Exception(s"We should not have a Number here! $pair")
        }
      }
      case (p: Pair, _: ActionDone) => reduceIt(p)
      case _ => throw new Exception(s"We should not have a Number here! $pair")
    }
  }
}
