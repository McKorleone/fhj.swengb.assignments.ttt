package fhj.swengb.assignments.ttt.pkoerner

import org.junit.Assert._
import org.junit.Test

/**
  * Tests the tic tac toe game engine.
  */
class TicTacToeTest {

  /**
    * contains all possible games as keys and the according game.
    */
  lazy val allGames: Map[Seq[TMove], TicTacToe] = TicTacToe.mkGames()

  @Test def setOnEmpty(): Unit = {
    val t = TicTacToe().turn(TopCenter, PlayerA)
    assertEquals(8, t.remainingMoves.size)
  }

  @Test def showPossibleGames(): Unit = {
    val allGames = Map[Seq[TMove],TicTacToe](
      Seq() ->  TicTacToe(Map(), PlayerA),
      Seq(MiddleCenter) ->  TicTacToe(Map(MiddleCenter -> PlayerB), PlayerA),
      Seq(TopLeft) ->  TicTacToe(Map(TopLeft -> PlayerB), PlayerA),
      Seq(TopRight) ->  TicTacToe(Map(TopRight -> PlayerB), PlayerA),
      Seq(TopCenter) ->  TicTacToe(Map(TopCenter -> PlayerB), PlayerA),
      Seq(MiddleLeft) ->  TicTacToe(Map(MiddleLeft -> PlayerB), PlayerA),
      Seq(MiddleRight) ->  TicTacToe(Map(MiddleRight -> PlayerB), PlayerA),
      Seq(BottomCenter) ->  TicTacToe(Map(BottomCenter -> PlayerB), PlayerA),
      Seq(BottomLeft) ->  TicTacToe(Map(BottomLeft -> PlayerB), PlayerA),
      Seq(BottomRight) ->  TicTacToe(Map(BottomRight -> PlayerB), PlayerA))

    assertEquals(allGames, TicTacToe.mkGames())
  }

  @Test def testMovesRemaining(): Unit = {
    val game = TicTacToe(
      Map(
        TopLeft -> PlayerB,
        TopCenter -> PlayerA,
        BottomRight -> PlayerB
      )
    )
    var remaining = Set(TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter)

    assertEquals(game.remainingMoves.size, 6)
    assertEquals(game.remainingMoves, remaining)
  }

  @Test def testGameOver(): Unit = {
    assertFalse(TicTacToe().gameOver)
  }

  @Test def testGameOver2(): Unit = {
    val gameOver = TicTacToe(
      Map(
        TopLeft -> PlayerB,
        TopCenter -> PlayerA,
        MiddleCenter -> PlayerB,
        TopRight -> PlayerA,
        BottomRight -> PlayerB
      )
    ).gameOver
    assertTrue(gameOver)
  }

  @Test def testGameOver3(): Unit = {
    val gameOver = TicTacToe(
      Map(
        TopLeft -> PlayerB,
        TopCenter -> PlayerA,
        MiddleCenter -> PlayerB,
        TopRight -> PlayerA
      )
    ).gameOver
    assertFalse(gameOver)
  }

  @Test def testGameOver4(): Unit = {
    val gameOver = TicTacToe(
      Map(
        TopLeft -> PlayerB,
        TopCenter -> PlayerA,
        TopRight -> PlayerB,
        MiddleLeft -> PlayerA,
        MiddleCenter -> PlayerB,
        MiddleRight -> PlayerA,
        BottomLeft -> PlayerB,
        BottomCenter -> PlayerA,
        BottomRight -> PlayerB
      )
    ).gameOver
    assertTrue(gameOver)
  }

  // implement yourself more tests


}
