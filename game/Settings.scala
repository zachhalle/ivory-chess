package game

import Chess.{Player, Maxie, Minnie}

object Settings {
  
  var MinnieIsAI = false
  var MaxieIsAI = false
  
  var searchDepth = 4
  
  def setPlayerIsAI(player: Player, isAI: Boolean) {
    player match {
      case Maxie => MaxieIsAI = isAI
      case Minnie => MinnieIsAI = isAI
    }
  }
  
  def playerIsAI(player: Player) = player match {
    case Maxie => MaxieIsAI
    case Minnie => MinnieIsAI
  }
  
}