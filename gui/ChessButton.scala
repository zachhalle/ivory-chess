package gui

import game._
import game.Chess._
import javax.swing._
import java.awt._
import java.awt.event._

class ChessButton(var piece: Option[Piece], 
				  val defaultColor: java.awt.Color, 
				  val posn: Position,
				  val disp: Display) extends JButton {
  
  private val selectedColor = Color.red
  private val canMoveColor = Color.orange
  private var locked = false
  
  setBackground(defaultColor)
  // setIcon(Display.pieceToImage(piece))
  addActionListener(new ActionListener() {
    override def actionPerformed(ae: ActionEvent) = locked match {
      case false => performAction()
      case true => 
    }
  })
  
  def lock() { locked = true }
  def unlock() { locked = false }
  
  private def performAction() = piece match {
    case None => { 
        if (disp.selected == None) {
          // do nothing
        } else { 
          
          val move = (disp.board, { val Some((r,c)) = disp.selected; (r,c) }, posn)
          val (_,begin,_) = move
          
          if (isLegalMove(move, disp.gameState)) {
            val Some((r,c)) = disp.selected
            disp.logger.log(move)
            disp.gameState = applyMove((disp.board, (r,c), posn), disp.gameState)
            disp.updateBoard(disp.gameState)
            disp.clearSelected()
          } else {
            disp.clearSelected()
          } 
        }
      }
      
    case Some(piece) => {
        
        if (disp.selected == None) {  
          if (!(piece colorMatchesPlayer player(disp.gameState))) {
            // do nothintg -- this piece cannot move this turn
          } else {
            disp.selected = Some(posn)
            setBackground(selectedColor)
          
            piece allMovesFrom (disp.board, posn) map {
              case (_,_,end) => end
            } foreach {
              case (r,c) => {
                disp.getButton(r,c).setBackground(canMoveColor)
              }
            }
          }
          
        } else { 
          
          val Some((r,c)) = disp.selected
          val Some(selectedPiece) = disp.board(r)(c) 
          
          if (piece.color != selectedPiece.color) {
            
            val move = (disp.board, (r,c), posn)
            val begin = (r,c)
          
            if (isLegalMove(move, disp.gameState)) {
        
                disp.logger.log(move)
                disp.gameState = applyMove((disp.board, (r,c), posn), disp.gameState)          
                disp.updateBoard(disp.gameState)
            
                if (gameOver(disp.gameState)) {
                  disp.gameOver()
                }
            
                disp.clearSelected()
            
            } else {
                disp.clearSelected()
            } 
          } else {
            disp.clearSelected()
            // redo selected process
            disp.selected = Some(posn)
            setBackground(selectedColor)
          
            piece allMovesFrom (disp.board, posn) map {
              case (_,_,end) => end
            } foreach {
              case (r,c) => {
                disp.getButton(r,c).setBackground(canMoveColor)
              }
            }
          }
        }
      } 
  }
}