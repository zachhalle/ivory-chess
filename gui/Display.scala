package gui

import game._
import game.Chess._
import javax.swing._
import java.awt._
import java.awt.event._
import ai.Engine

class Display private (var selected: Option[Position], var gameState: State) extends JFrame("Ivory") {

  def this() = this(None, start())
  
  val logger = new DebuggerWindow()
  
  val panel = new JPanel(new GridLayout(8,8))
  val boardGUI = Array.ofDim[ChessButton](8,8)
 
  def board = Chess.board(gameState)
  def player = Chess.player(gameState)
  
  init()
  
  def init() {
    
    this add panel
    
    val color1 = Color.white
    val color2 = Color.gray
    
    for (r <- 0 until 8)
      for (c <- 0 until 8) {
        boardGUI(r)(c) = new ChessButton(board(r)(c), 
            if ((r+c) % 2 == 0) color1 else color2, (r,c),
            this)
        
        boardGUI(r)(c) setIcon pieceToImage(board(r)(c))
        panel add boardGUI(r)(c)
      }
    
    setSize(600,600)
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setResizable(false)
    new PlayerSelectionWindow(this)
 
  }
  
  def updateBoard(state: State) {
    
    for (r <- 0 until 8)
      for (c <- 0 until 8) {
        val piece = board(r)(c)
        boardGUI(r)(c).piece = piece
        boardGUI(r)(c).setIcon(pieceToImage(piece))
      }
    
    if (Chess.gameOver(state)) {
      gameOver()
    } else {
      
      val move = Engine.computeMove(state)
      gameState = applyMove(move, state)
      
      for (r <- 0 until 8)
      for (c <- 0 until 8) {
        val piece = board(r)(c)
        boardGUI(r)(c).piece = piece
        boardGUI(r)(c).setIcon(pieceToImage(piece))
      }
      
      if (Chess.gameOver(gameState)) {
        gameOver()
      }
      
    }
    
  }
  
  def pieceToImage(piece: Option[Piece]): ImageIcon = piece match {
    case Some(piece) => ImageReference(piece)
    case None => null
  }
  
  def clearSelected() { 
    selected = None 
    for (r <- 0 until 8)
      for (c <- 0 until 8) {
        boardGUI(r)(c).setBackground(boardGUI(r)(c).defaultColor)
      }
    
  }
  
  def gameOver() {
    
    for (r <- 0 until 8)
      for (c <- 0 until 8) {
        boardGUI(r)(c).lock()
      }
    
    JOptionPane.showMessageDialog(new JFrame("Game over"), 
        status(gameState) match {
          case Over(Winner(Maxie)) => "Checkmate! White wins."
          case Over(Winner(Minnie)) => "Checkmate! Black wins."
          case Over(Tie()) => "Stalemate! It's a tie."
          case _ => throw new GameException("The game should be over.")
    })
    
  }

  def getButton(r: Int, c: Int) = boardGUI(r)(c)
  def getPiece(r: Int, c: Int) = board(r)(c)
  override def dispose() {
    logger.dispose()
    super.dispose()
  }
  
}

object Display {

  def debugView(brd: Board) {
    for (r <- 0 until 8) {
      var str = ""
      for (c <- 0 until 8) {
        System.out.print (brd(r)(c) match {
          case None => str += " "
          case Some(Rook(White,_)) => str += "r"
          case Some(Bishop(White)) => str += "b"
          case Some(Queen(White)) => str += "q"
          case Some(Knight(White)) => str += "h"
          case Some(Pawn(White,_)) => str += "p"
          case Some(King(White,_)) => str += "k"
          case Some(Rook(Black,_	)) => str += "R"
          case Some(Bishop(Black)) => str += "B"
          case Some(Queen(Black)) => str += "Q"
          case Some(Knight(Black)) => str += "H"
          case Some(Pawn(Black,_)) => str += "P"
          case Some(King(Black,_)) => str += "K"
        })
        
        str += " | "
      }
      println(str)
      println("------------------------------")
    }
  }
}