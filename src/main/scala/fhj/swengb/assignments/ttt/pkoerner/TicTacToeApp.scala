package fhj.swengb.assignments.ttt.pkoerner

import javafx.application.Application
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Label, Button}
import javafx.scene.{Scene, Parent}
import javafx.stage.Stage

import scala.util.control.NonFatal

/**
  * Implement here your TicTacToe JavaFX App.
  */

object TicTacToeApp {
  def main(args: Array[String]) {
    Application.launch(classOf[TicTacToeApp], args: _*)
  }
}

class TicTacToeApp extends Application {

  val Fxml = "/fhj/swengb/assignments/ttt/TicTacToeApp.fxml"

  val loader = new FXMLLoader(getClass.getResource(Fxml))

  override def start(stage: Stage): Unit = try {
    stage.setTitle("Tic Tac Toe")
    loader.load[Parent]() // side effect
    val scene = new Scene(loader.getRoot[Parent])
    stage.setScene(scene)
    stage.show()

  } catch {
    case NonFatal(e) => {
      e.printStackTrace()
    }
  }

}

class TicTacToeAppController {

  @FXML private var btn_TL: Button = _
  @FXML private var btn_TC: Button = _
  @FXML private var btn_TR: Button = _
  @FXML private var btn_ML: Button = _
  @FXML private var btn_MC: Button = _
  @FXML private var btn_MR: Button = _
  @FXML private var btn_BL: Button = _
  @FXML private var btn_BC: Button = _
  @FXML private var btn_BR: Button = _
  @FXML private var lbl_plA: Label = _
  @FXML private var lbl_plB: Label = _

  var ticTacToeGame = TicTacToe()
  var gameMap: Map[TMove, Player] = Map()

  def gameUpdate(allButtons: Button, currentGame: TicTacToe): TicTacToe = {
    allButtons.setText(if (currentGame.nextPlayer == PlayerA) "x" else "o")
    val buttonID = allButtons.getId
    val moveDone: TMove = buttonID match {
      case "btn_TL" => TopLeft
      case "btn_TC" => TopCenter
      case "btn_TR" => TopRight
      case "btn_ML" => MiddleLeft
      case "btn_MC" => MiddleCenter
      case "btn_MR" => MiddleRight
      case "btn_BL" => BottomLeft
      case "btn_BC" => BottomCenter
      case "btn_BR" => BottomRight
    }

    gameMap += (moveDone -> currentGame.nextPlayer)

    val gameUpdate = currentGame.turn(moveDone, currentGame.nextPlayer)

    if (gameUpdate.gameOver) {
      var amountPlayerA: Seq[Player] = Seq()
      var amountPlayerB: Seq[Player] = Seq()

      btn_TL.setDisable(true)
      btn_TC.setDisable(true)
      btn_TR.setDisable(true)
      btn_ML.setDisable(true)
      btn_MC.setDisable(true)
      btn_MR.setDisable(true)
      btn_BL.setDisable(true)
      btn_BC.setDisable(true)
      btn_BR.setDisable(true)

      for ((move, player) <- gameMap) {
        if (player == PlayerA) amountPlayerA = amountPlayerA :+ player
        else amountPlayerB = amountPlayerB :+ player
      }

        if (amountPlayerA.size == 3) {
          lbl_plA.setText("Player A WINS!!!")
          lbl_plB.setText("Sorry... You lost!!!")
        } else if (amountPlayerB.size == 3) {
          lbl_plA.setText("Sorry... You lost!!!")
          lbl_plB.setText("Player B WINS!!!")
        } else {
          lbl_plA.setText("TIE!!")
          lbl_plB.setText("TIE!!")
        }
    }
    gameUpdate
  }



  def TL(): Unit = {
    ticTacToeGame = gameUpdate(btn_TL, ticTacToeGame)
  }

  def TC(): Unit = {
    ticTacToeGame = gameUpdate(btn_TC, ticTacToeGame)
  }

  def TR(): Unit = {
    ticTacToeGame = gameUpdate(btn_TR, ticTacToeGame)
  }

  def ML(): Unit = {
    ticTacToeGame = gameUpdate(btn_ML, ticTacToeGame)
  }

  def MC(): Unit = {
    ticTacToeGame = gameUpdate(btn_MC, ticTacToeGame)
  }

  def MR(): Unit = {
    ticTacToeGame = gameUpdate(btn_MR, ticTacToeGame)
  }

  def BL(): Unit = {
    ticTacToeGame = gameUpdate(btn_BL, ticTacToeGame)
  }

  def BC(): Unit = {
    ticTacToeGame = gameUpdate(btn_BC, ticTacToeGame)
  }

  def BR(): Unit = {
    ticTacToeGame = gameUpdate(btn_BR, ticTacToeGame)
  }

  def restart(): Unit = {
    ticTacToeGame = TicTacToe()
    lbl_plA.setText("Player A")
    lbl_plB.setText("Player B")
    btn_TL.setText("-")
    btn_TC.setText("-")
    btn_TR.setText("-")
    btn_ML.setText("-")
    btn_MC.setText("-")
    btn_MR.setText("-")
    btn_BL.setText("-")
    btn_BC.setText("-")
    btn_BR.setText("-")
    btn_TL.setDisable(false)
    btn_TC.setDisable(false)
    btn_TR.setDisable(false)
    btn_ML.setDisable(false)
    btn_MC.setDisable(false)
    btn_MR.setDisable(false)
    btn_BL.setDisable(false)
    btn_BC.setDisable(false)
    btn_BR.setDisable(false)
  }
}
