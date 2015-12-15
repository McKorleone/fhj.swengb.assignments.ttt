package fhj.swengb.assignments.ttt.pkoerner

import scala.collection.Set

/**
  * models the different moves the game allows
  *
  * each move is made by either player a or player b.
  */
sealed trait TMove {
  def idx: Int
}

case object TopLeft extends TMove {
  override def idx: Int = 0
}

case object TopCenter extends TMove {
  override def idx: Int = 1
}

case object TopRight extends TMove {
  override def idx: Int = 2
}

case object MiddleLeft extends TMove {
  override def idx: Int = 3
}

case object MiddleCenter extends TMove {
  override def idx: Int = 4
}

case object MiddleRight extends TMove {
  override def idx: Int = 5
}

case object BottomLeft extends TMove {
  override def idx: Int = 6
}

case object BottomCenter extends TMove {
  override def idx: Int = 7
}

case object BottomRight extends TMove {
  override def idx: Int = 8
}


/**
  * for a tic tac toe game, there are two players, player A and player B
  */
sealed trait Player

case object PlayerA extends Player

case object PlayerB extends Player

case object NoPlayer extends Player

object TicTacToe {

  /**
    * creates an empty tic tac toe game
    *
    * @return
    */
  def apply(): TicTacToe = TicTacToe(Map())

  /**
    * For a given tic tac toe game, this function applies all moves to the game.
    * The first element of the sequence is also the first move.
    *
    * @param t
    * @param moves
    * @return
    */
  def play(t: TicTacToe, moves: Seq[TMove]): TicTacToe = ???

  /**
    * creates all possible games.
    *
    * @return
    */
  def mkGames(): Map[Seq[TMove], TicTacToe] = {

    val possibleMoves = Seq(TopLeft, TopCenter, TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)

    //default entry for no move
    var gameMap: Map[Seq[TMove], TicTacToe] = Map(Seq() -> TicTacToe(Map(), PlayerA))

    //add all possible games to map
    for (move <- possibleMoves) {
      gameMap += (Seq(move) -> TicTacToe(Map(move -> PlayerA), PlayerB))
    }
    gameMap
  }
}

/**
  * Models the well known tic tac toe game.
  *
  * The map holds the information which player controls which field.
  *
  * The nextplayer parameter defines which player makes the next move.
  */
case class TicTacToe(moveHistory: Map[TMove, Player],
                     nextPlayer: Player = PlayerA) {

  /**
    * outputs a representation of the tic tac toe like this:
    *
    * |---|---|---|
    * | x | o | x |
    * |---|---|---|
    * | o | x | x |
    * |---|---|---|
    * | x | o | o |
    * |---|---|---|
    *
    * @return
    */
  def asString(): String = {

    //create Map with Index (TMove) and Value (Position of move)
    val indexMap = Map(0 -> 16, 1 -> 20, 2 -> 24,
                       3 -> 44, 4 -> 48, 5 -> 52,
                       6 -> 72, 7 -> 76, 8 -> 80)

    //create playground TicTacToe
    var ticTacToe: String =
        "|---|---|---|\n" +
        "|   |   |   |\n" +
        "|---|---|---|\n" +
        "|   |   |   |\n" +
        "|---|---|---|\n" +
        "|   |   |   |\n" +
        "|---|---|---|\n"


    //insert in playground x for Player A, o for Player B or "empty" for not yet visited
    for ((indexTicTacToe, playerPlayed) <- moveHistory) {
      if (playerPlayed == PlayerA) {
        ticTacToe = ticTacToe.updated(indexMap(indexTicTacToe.idx), "x").mkString
      }
      else if (playerPlayed == PlayerB) {
        ticTacToe = ticTacToe.updated(indexMap(indexTicTacToe.idx), "o").mkString
      }
      else {
        ticTacToe = ticTacToe.updated(indexMap(indexTicTacToe.idx), " ").mkString
      }
    }
    ticTacToe
  }

  /**
    * is true if the game is over.
    *
    * The game is over if either of a player wins or there is a draw.
    */
  val gameOver: Boolean = {
    if (moveHistory.size > 8 || winner != None) {
      true
    } else {
      false
    }
  }

  /**
    * the moves which are still to be played on this tic tac toe.
    */
  val remainingMoves: Set[TMove] = {
    var movesRemainig = Seq(TopLeft, TopCenter, TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)

  //Fehlt noch!!!

  }


  /**
    * given a tic tac toe game, this function returns all
    * games which can be derived by making the next turn. that means one of the
    * possible turns is taken and added to the set.
    */
  lazy val nextGames: Set[TicTacToe] = ???

  /**
    * Either there is no winner, or PlayerA or PlayerB won the game.
    *
    * The set of moves contains all moves which contributed to the result.
    */
  def winner: Option[(Player, Set[TMove])] = {

    val allMoves = Seq(TopLeft, TopCenter, TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)

    //extrcat all valid combinations
    if (moveHistory.values == PlayerA) {
      Option(PlayerA, Set[TMove]())
    } else if (moveHistory.values == PlayerB) {
      Option(PlayerB, Set[TMove]())
    } else {
      Option(NoPlayer, Set[TMove]())
    }
  }

  /**
    * returns a copy of the current game, but with the move applied to the tic tac toe game.
    *
    * @param move   to be played
    * @param player the player
    * @return
    */
  def turn(p: TMove, player: Player): TicTacToe = ???

}


