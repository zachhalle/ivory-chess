package game

trait Game {
  
  sealed abstract class Player
  case object Maxie extends Player
  case object Minnie extends Player

  sealed abstract class Outcome
  case class Winner(p: Player) extends Outcome
  case class Tie extends Outcome
  
  sealed abstract class Status
  case class Over(oc: Outcome) extends Status
  case class InPlay extends Status
  
  type State //(board, player to move, turn)
  type Move
  
  class GameException(s: String) extends Exception(s)
  
  //interface code with State
  def moves (s: State): Vector[Move]
  def status (s: State): Status
  def turn (s: State): Int
  def player (s: State): Player
  def gameOver (s: State) = status(s) match {
    case InPlay() => false
    case _ => true
  }
  
  //init state
  def start(): State
  
  //applying moves to states
  def isLegalMove (m: Move, s: State): Boolean
  def applyMove (m: Move, s: State): State
 
}