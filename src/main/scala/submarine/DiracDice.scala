package submarine

import scala.annotation.tailrec

case class Player(id: Int, position: Int, score: Int = 0, turns: Int = 0) {
  def playADeterministicTurn(dice: DeterministicDice): (Player, DeterministicDice) = {
    val (diceAfterTurn, results) = (1 to 3).foldLeft((dice, Seq.empty[Int]))((diceAndResults, _) => diceAndResults match {
      case (d: DeterministicDice, results) =>
        val rolled = d.roll
        (rolled, results ++ Seq(rolled.face))
    })
    val newPosition = 1 + (position + results.sum - 1) % 10
    (this.copy(position = newPosition, score = score + newPosition, turns = turns + 1), diceAfterTurn)
  }

  def playADiracTurn(): Seq[Player] = {
    (1 to 3).foldLeft(Seq((TheDiracDice(), Seq.empty[Int])))((universes, _) => {
      universes.flatMap {
        case (d: TheDiracDice, results) =>
          d.roll.map(rolled => (rolled, results ++ Seq(rolled.face)))
      }
    }).map {
      case (_, results) =>
        val newPosition = 1 + (position + results.sum - 1) % 10
        this.copy(position = newPosition, score = score + newPosition)
    }

  }

  def winsADeterministicGame: Boolean = score >= 1000

  def winsADiracGame: Boolean = score >= 21
}

case class DeterministicDice(face: Int = 0, timesRolled: Int = 0) {
  def roll: DeterministicDice = DeterministicDice(face = face + 1, timesRolled = timesRolled + 1)
}

case class TheDiracDice(face: Int = 0) {
  def roll: Seq[TheDiracDice] = Seq(
    TheDiracDice(face = 3),
    TheDiracDice(face = 2),
    TheDiracDice(face = 1)
  )
}

case class GameResult(winner: Player, loser: Player)

case class Universe(player1: Player, player2: Player, winner: Option[Player] = None)

object DiracDice {
  def parse(representation: String): Player = {
    val player = raw"Player (\d) starting position: (\d)".r
    representation match {
      case player(id, position) => Player(id.toInt, position.toInt)
    }
  }

  @tailrec
  def playWithDeterministicDice(player1: Player, player2: Player, dice: DeterministicDice): (GameResult, DeterministicDice) = {
    val (player1AfterThisTurn, diceAfterPlayer1Turn) = player1.playADeterministicTurn(dice)
    if (player1AfterThisTurn.winsADeterministicGame)
      (GameResult(player1AfterThisTurn, player2), diceAfterPlayer1Turn)
    else {
      val (player2AfterThisTurn, diceAfterPlayer2Turn) = player2.playADeterministicTurn(diceAfterPlayer1Turn)
      if (player2AfterThisTurn.winsADeterministicGame)
        (GameResult(player2AfterThisTurn, player1AfterThisTurn), diceAfterPlayer2Turn)
      else
        playWithDeterministicDice(player1AfterThisTurn, player2AfterThisTurn, diceAfterPlayer2Turn)
    }
  }

  @tailrec
  def playWithDiracDice(playingUniverses: Seq[(Universe, BigInt)], winners: Map[Int, BigInt] = Map.empty): Map[Int, BigInt] = {
    if (playingUniverses.isEmpty) winners
    else {
      val universesAndTimesAppearsAfterThisTurn = playingUniverses.flatMap(universeAndTimesAppears => {
        val (universe, timesAppears) = universeAndTimesAppears
        val newPlayer1Universes = universe.player1.playADiracTurn()
        newPlayer1Universes.flatMap(player1AfterThisTurn => {
          if (player1AfterThisTurn.winsADiracGame)
            Seq((Universe(player1AfterThisTurn, universe.player2, Some(player1AfterThisTurn)), timesAppears))
          else {
            val newPlayer2Universes = universe.player2.playADiracTurn()
            newPlayer2Universes.map(player2AfterThisTurn => {
              if (player2AfterThisTurn.winsADiracGame)
                (Universe(player1AfterThisTurn, player2AfterThisTurn, Some(player2AfterThisTurn)), timesAppears)
              else
                (Universe(player1AfterThisTurn, player2AfterThisTurn), timesAppears)
            })
          }
        })
      })
      val universesAfterThisTurn = universesAndTimesAppearsAfterThisTurn.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap
      val gameIsStillInProgress = universesAfterThisTurn.view.filterKeys(_.winner.isEmpty).toSeq
      val newWinners = universesAfterThisTurn
        .view.filterKeys(_.winner.isDefined)
        .toSeq
        .groupBy(_._1.winner.get.id)
        .view
        .mapValues(_.map(_._2).sum)
      playWithDiracDice(
        gameIsStillInProgress,
        winners ++ newWinners.map {
          case (p, l) => (p, winners.getOrElse(p, BigInt(0)) + l)
        }
      )
    }
  }
}
