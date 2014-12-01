package gui

import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.JLabel
import javax.swing.JRadioButton
import javax.swing.JButton
import javax.swing.ButtonGroup
import java.awt.event.{ActionListener, ActionEvent}
import java.awt.GridLayout
import game.Settings
import game.Chess.{Player, Minnie, Maxie}

class PlayerSelectionWindow(display: Display) extends JFrame("Ivory") {
  
  val panel = new JPanel(new GridLayout(4,1))
  
  panel add new JLabel("Welcome to Ivory!")
  panel add new JLabel("Select players:")
  
  val playerSelectionPanel = new JPanel(new GridLayout(1,2))
  
  val whiteSelectionPanel = new JPanel(new GridLayout(3,1))
  val blackSelectionPanel = new JPanel(new GridLayout(3,1))
  
  //init white panel
  
  whiteSelectionPanel add new JLabel("White:")
  
  val whiteIsHuman = new JRadioButton("Human")
  whiteIsHuman setSelected true
  whiteIsHuman setActionCommand "Human"
  
  val whiteIsAI = new JRadioButton("AI")
  whiteIsAI setActionCommand "AI"
  
  val whiteSelectionGroup = new ButtonGroup
  whiteSelectionGroup add whiteIsHuman
  whiteSelectionGroup add whiteIsAI
  
  whiteSelectionPanel add whiteIsHuman
  whiteSelectionPanel add whiteIsAI
  
  //init black panel
  
  blackSelectionPanel add new JLabel("Black:")
  
  val blackIsHuman = new JRadioButton("Human")
  blackIsHuman setActionCommand "Human"
  
  val blackIsAI = new JRadioButton("AI")
  blackIsAI setSelected true
  blackIsHuman setActionCommand "AI"
  
  val blackSelectionGroup = new ButtonGroup
  blackSelectionGroup add blackIsHuman
  blackSelectionGroup add blackIsAI
  
  blackSelectionPanel add blackIsHuman
  blackSelectionPanel add blackIsAI
  
  playerSelectionPanel add whiteSelectionPanel
  playerSelectionPanel add blackSelectionPanel
  
  // add selectors
  panel add playerSelectionPanel
  
  // button
  
  val startButton = new JButton("Start")
  startButton.addActionListener(new ActionListener() {
    override def actionPerformed(ae: ActionEvent) {
      
      if (blackIsAI.isSelected()) {
        Settings.setPlayerIsAI(Minnie, true)
      } else if (blackIsHuman.isSelected()) {
        Settings.setPlayerIsAI(Minnie, false)
      } else throw new Exception
      
      if (whiteIsAI.isSelected()) {
        Settings.setPlayerIsAI(Maxie, true)
      } else if (whiteIsHuman.isSelected()) {
        Settings.setPlayerIsAI(Minnie, false)
      } else throw new Exception
      
      display.setVisible(true)
      dispose()
      
    }
  })
  
  panel add startButton
  
  add(panel)
  setSize(270,300)
  setResizable(false)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setVisible(true)
  
}