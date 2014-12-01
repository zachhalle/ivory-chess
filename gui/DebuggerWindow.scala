package gui

import javax.swing.JFrame
import javax.swing.JButton
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.WindowConstants
import collection.mutable.Queue
import game.Chess.Position
import game.Chess.Move

class DebuggerWindow private (val moves: Queue[(Position, Position)]) extends JFrame {

  def this() = this(new Queue[(Position, Position)])
  
  val button = new JButton("Log!")
  button.addActionListener(new ActionListener() {
    override def actionPerformed(ae: ActionEvent) {
      moves foreach println
    }
  })
  
  this add button
  this setSize (200,200)
  this setDefaultCloseOperation WindowConstants.DISPOSE_ON_CLOSE
  this setResizable false
  this setVisible true
  
  def log(m: Move) {
    m match {
      case (_, init, end) => moves enqueue ((init,end))
    }
  }
  
}