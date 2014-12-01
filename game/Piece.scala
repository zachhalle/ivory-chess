package game
import Chess._
import scala.annotation.tailrec

sealed abstract class Color
case object Black extends Color
case object White extends Color

class Unimplemented extends Exception

sealed abstract class Piece (val color: Color) {
  
  final def colorMatchesPlayer(p: Player) = p match {
    case Maxie => this.color == White
    case Minnie => this.color == Black
  }
  
  //whether the change in location is kosher regardless of check bullshit
  //and you don't land on your own piece
  def canMakeMove (move: Move): Boolean
  def allCanMakeMoves(board: Board, initPos: Position): Vector[Move] 
  //all moves such that the change is kosher, considering check bullshit
  final def allMovesFrom (board: Board, initPos: Position): Vector[Move] = {
    filterCheckMoves(allCanMakeMoves(board, initPos))
  }

  final def endPosnNotOccupied(board: Board, endPos: Position): Boolean = {
    val (r,c) = endPos
    board(r)(c) match {
      case Some(piece: Piece) if piece.color == color => false
      case _ => true
    }
  }
  
  final def playerFromColor(color: Color) = color match {
    case White => Maxie
    case Black => Minnie
  }
  
  final def filterCheckMoves(moves: Vector[Move]): Vector[Move] = {
    val player = playerFromColor(color)
    val filtered = moves filter ((move) => {
      val (board, _, _) = move
      !currentPlayerInCheck(Chess.board(applyMove(move, (board, 0, player))), player)
    })
    filtered
  }
  
}

case class King(override val color: Color, val unmoved: Boolean) extends Piece(color) {
  
  def canMakeMove (move: Move): Boolean = {
    val (board, (init_r, init_c), (end_r, end_c)) = move
    //should be a king in the start position
    (board(init_r)(init_c) match { 
      case Some(King(c,_)) if color == c => true
      case _ => false
    }) &&
    //abs row diff should be at most 1
    (((Math.abs(init_r-end_r) <= 1) && 
    //abs col diff should be at most 1
    (Math.abs(init_c-end_c) <= 1) &&
    //but at least one should change
    (init_r, init_c) != (end_r,end_c) &&
    //should end within the board
    end_r >= 0 && end_c >= 0 &&
    end_r < 8 && end_c < 8) ||
    //empty or capture opponent
    (if (unmoved && !currentPlayerInCheck(board, playerFromColor(color))) { // castling is a pain in the ass
      color match {
        case White => {
          if ((end_r, end_c) == (7,6)) {
            board(7)(7) match {
              case Some(rook: Rook) if rook.color == White && rook.unmoved => {
                (board(7)(5) == None && board(7)(6) == None) &&
                !currentPlayerInCheck(Chess.board(applyMove((board, (7,4), (7,5)), (board, 0, Maxie))), Maxie)
              }
              case _ => false
            }
          } else if ((end_r, end_c) == (7,2)) {
            board(7)(0) match {
              case Some(rook: Rook) if rook.color == White && rook.unmoved => {
                (board(7)(1) == None && board(7)(2) == None && board(7)(3) == None) &&
                !currentPlayerInCheck(Chess.board(applyMove((board, (7,4), (7,3)), (board, 0, Maxie))), Maxie) &&
                !currentPlayerInCheck(Chess.board(applyMove((board, (7,4), (7,2)), (board, 0, Maxie))), Maxie)
              }
              case _ => false
            }
          } else false
        }
        case Black => {
          if ((end_r, end_c) == (0,6)) {
            board(0)(7) match {
              case Some(rook: Rook) if rook.color == Black && rook.unmoved => {
                (board(0)(5) == None && board(0)(6) == None) &&
                !currentPlayerInCheck(Chess.board(applyMove((board, (0,4), (0,5)), (board, 0, Minnie))), Minnie)
              }
              case _ => false
            }
          } else if ((end_r, end_c) == (0,2)) {
            board(0)(0) match {
              case Some(rook: Rook) if rook.color == Black && rook.unmoved => {
                (board(0)(1) == None && board(0)(2) == None && board(0)(3) == None) &&
                !currentPlayerInCheck(Chess.board(applyMove((board, (0,4), (0,2)), (board, 0, Minnie))), Minnie) &&
                !currentPlayerInCheck(Chess.board(applyMove((board, (0,4), (0,3)), (board, 0, Minnie))), Minnie)
              }
              case _ => false
            }
          } else false
        }
      } 
    } else false)) &&
    endPosnNotOccupied(board, (end_r, end_c))
  }
  
  def allCanMakeMoves (board: Board, initPos: Position): Vector[Move] = {
    
    val (init_r, init_c) = initPos
    
    board(init_r)(init_c) match {
      case Some(King(color,_)) if color == this.color =>
      case _ => return Vector.empty[Move]
    }
  
    val builder = Vector.newBuilder[Move]
    
    for (rowChange <- -1 to 1)
      for (colChange <- -1 to 1) {
        val (new_r, new_c) = (init_r + rowChange, init_c + colChange)
        if ((0,0) != (rowChange, colChange) &&
            ((0 until 8) contains new_c) &&
            ((0 until 8) contains new_r) &&
            endPosnNotOccupied(board, (new_r, new_c))) {
              builder += ((board, initPos, (new_r, new_c)))
        }
      }
    
    if (unmoved) {
      color match {
        case White => {
          if (canMakeMove(board, initPos, (7,6))) { builder += ((board, initPos, (7, 6))) }
          if (canMakeMove(board, initPos, (7,2))) { builder += ((board, initPos, (7, 2))) }
        }
        case Black => {
          if (canMakeMove(board, initPos, (0,6))) { builder += ((board, initPos, (0, 6))) }
          if (canMakeMove(board, initPos, (0,2))) { builder += ((board, initPos, (0, 2))) }
        }
      }
    }
    
    builder.result()
    
  }

}

case class Pawn(override val color: Color, val unmoved: Boolean) extends Piece(color) {
  
  def canMakeMove (move: Move): Boolean = {
    
    val (board, (init_r, init_c), (end_r, end_c)) = move
    // the init posn should be a pawn
    (board(init_r)(init_c) match {
      case Some(Pawn(color,_)) if color == this.color => true
      case _ => false
    }) &&
    // the end posn should be on the board
    end_r >= 0 && end_c >= 0 &&
    end_r < 8 && end_c < 8 &&
    (color match { 
      
      case White => {
        //white pieces should move up one row
        if (end_r - init_r == -1) {
          //if no col change, should be a non capture move
          if (end_c - init_c == 0) {
            board(end_r)(end_c) == None
          //if col changes by precisely 1, should be a capture
          } else if (math.abs(end_c - init_c) == 1) {
            board(end_r)(end_c) match {
              case Some(piece) if piece.color == Black => true
              case _ => false
            } 
          } else false 
        } else if (unmoved && end_r - init_r == -2 && end_c - init_c == 0) {
        	board(end_r)(end_c) == None
        } else false
      }
      
      case Black => { 
        // black pieces should move down one row
        if (end_r - init_r == 1) {
          if (end_c - init_c == 0) {
            board(end_r)(end_c) == None
          } else if (math.abs(end_c - init_c) == 1) {
            board(end_r)(end_c) match {
              case Some(piece) if piece.color == White => true
              case _ => false
            }
          } else false
        } else if (unmoved && end_r - init_r == 2 && end_c - init_c == 0) {
          board(end_r)(end_c) == None
        } else false
      }
      
    })
  
  }
  
  def allCanMakeMoves (board: Board, initPos: Position): Vector[Move] = {
    
    val (init_r, init_c) = initPos
    
    board(init_r)(init_c) match {
      case Some(Pawn(color,_)) if this.color == color => 
      case _ => return Vector.empty[Move]
    }
    
    val builder = Vector.newBuilder[Move]
    val new_r = if (color == Black) 
    			  init_r + 1
    			else
    			  init_r - 1
    			  
    for (colChange <- -1 to 1) {
      val new_c = init_c + colChange
      if (colChange == 0) {
        if (board(new_r)(new_c) == None)
          builder += ((board, initPos, (new_r, new_c)))
      } else if (((0 until 8) contains new_c)) {
        board(new_r)(new_c) match {
          case Some(piece) if piece.color != this.color => { builder += ((board, initPos, (new_r, new_c))) }
          case _ => 
        }
      }
    }
   
    if (unmoved) {
      color match {
        case White => 
          if (board(init_r - 2)(init_c) == None) 
            builder += ((board, initPos, (init_r - 2, init_c)))
        case Black => 
          if (board(init_r + 2)(init_c) == None) 
            builder += ((board, initPos, (init_r + 2, init_c)))
      }
    }  
    
    builder.result()
    
  }  

}

case class Rook(override val color: Color, val unmoved: Boolean) extends Piece(color) {
 
  def canMakeMove (move: Move): Boolean = {
    val (board, (init_r, init_c), (end_r, end_c)) = move
    // should be a rook in the initial position
    (board(init_r)(init_c) match {
      case Some(Rook(color,_)) if color == this.color => true
      case _ => false
    }) &&
    // should end on the board
    end_r >= 0 && end_c >= 0 &&
    end_r < 8 && end_c < 8 &&
    //if row is fixed
    (if (init_r == end_r) {
      // then check all intermediate col spaces are empty
       if (end_c > init_c) {
         ((init_c + 1) until end_c) forall ((i) => board(init_r)(i) == None)
       } else if (end_c < init_c) {
         ((end_c + 1) until init_c) forall ((i) => board(init_r)(i) == None)
       } else false // returning false since row and col are both fixed
    // if col is fixed
    } else if (init_c == end_c) {
      // then check all intermediate row spaces are empty
       if (end_r > init_r) {
         ((init_r + 1) until end_r) forall ((i) => board(i)(init_c) == None)
       } else if (end_r < init_r) {
         ((end_r + 1) until init_r) forall ((i) => board(i)(init_c) == None)
       } else false
    // return false because neither row nor col is fixed
    } else false) &&
    //end posn should not be occupied
    endPosnNotOccupied(board, (end_r, end_c))
  }
  
  def allCanMakeMoves (board: Board, initPos: Position): Vector[Move] = {
    
    val (init_r, init_c) = initPos
    
    board(init_r)(init_c) match {
      case Some(Rook(color,_)) if color == this.color => 
      case _ => return Vector.empty[Move]
    }
    
    val builder = Vector.newBuilder[Move]
    
    @tailrec
    def down (new_r: Int) {
      if (new_r >= 8)
        return
      board(new_r)(init_c) match {
        case None => { builder += ((board, initPos, (new_r, init_c))); down(new_r+1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, init_c)))
        case _ =>
      }
    }
    
    @tailrec
    def up (new_r: Int) {
      if (new_r < 0)
        return
      board(new_r)(init_c) match {
        case None => { builder += ((board, initPos, (new_r, init_c))); up(new_r-1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, init_c)))
        case _ =>
      }
    }
    
    @tailrec
    def left(new_c: Int) {
      if (new_c < 0)
        return
      board(init_r)(new_c) match {
        case None => { builder += ((board, initPos, (init_r, new_c))); left(new_c-1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (init_r, new_c)))
        case _ =>
      }
    }
    
    @tailrec
    def right(new_c: Int) {
      if (new_c >= 8)
        return
      board(init_r)(new_c) match {
        case None => { builder += ((board, initPos, (init_r, new_c))); right(new_c+1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (init_r, new_c)))
        case _ =>
      }
    }
    
    up(init_r-1)
    down(init_r+1)
    left(init_c-1)
    right(init_c+1)
    
    builder.result()
    
  }

}

case class Bishop(override val color: Color) extends Piece(color) {
  
  def canMakeMove (move: Move): Boolean = {
  
    val (board, (init_r, init_c), (end_r, end_c)) = move
    // should be a bishop in the initial state
    board(init_r)(init_c) == Some(Bishop(color)) &&
    // should end on the board
    end_r >= 0 && end_c >= 0 &&
    end_r < 8 && end_c < 8 &&
    endPosnNotOccupied(board, (end_r, end_c)) &&
    // slope should be 1 or -1
    (if (end_r - init_r == end_c - init_c) {
      if (init_r < end_r) {
        // intermediate spaces are empty
        (for (i <- 1 until (end_r-init_r)) yield (init_r + i, init_c + i)) forall 
          { case (r,c) => board(r)(c) == None }
      } else if (init_r > end_r) { 
        // intermediate spaces are empty
        (for (i <- 1 until (init_r-end_r)) yield (end_r + i, end_c + i)) forall
          { case (r,c) => board(r)(c) == None }
      } else false // implies no movement
    } else if (end_r - init_r == -(end_c - init_c)) {
        if (init_r > end_r) {
          (for (i <- 1 until (init_r-end_r)) yield (end_r + i, end_c - i)) forall 
            { case (r,c) => board(r)(c) == None }
        } else if (init_r < end_r) {
          (for (i <- 1 until (end_r-init_r)) yield (init_r + i, init_c - i)) forall
            { case (r,c) => board(r)(c) == None}
        } else false
    } else false)
  
  }
  
  def allCanMakeMoves (board: Board, initPos: Position): Vector[Move] = {
    
    val (init_r, init_c) = initPos
    
    if (board(init_r)(init_c) != Some(Bishop(color)))
      return Vector.empty[Move]
    
    val builder = Vector.newBuilder[Move]
    
    @tailrec
    def upleft(new_r: Int, new_c: Int) {
      if (new_r < 0 || new_c < 0)
        return
      board(new_r)(new_c) match {
        case None => { builder += ((board, initPos, (new_r, new_c))); upleft(new_r-1,new_c-1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, new_c)))
        case _ =>
      } 
    }
    
    @tailrec
    def upright(new_r: Int, new_c: Int) {
      if (new_r < 0 || new_c >= 8)
        return
      board(new_r)(new_c) match {
        case None => { builder += ((board, initPos, (new_r, new_c))); upright(new_r-1,new_c+1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, new_c)))
        case _ =>
      } 
    }
    
    @tailrec
    def downleft(new_r: Int, new_c: Int) {
      if (new_r >= 8 || new_c < 0)
        return
      board(new_r)(new_c) match {
        case None => { builder += ((board, initPos, (new_r, new_c))); downleft(new_r+1,new_c-1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, new_c)))
        case _ =>
      } 
    }
    
    @tailrec
    def downright(new_r: Int, new_c: Int) {
      if (new_r >= 8 || new_c >= 8)
        return
      board(new_r)(new_c) match {
        case None => { builder += ((board, initPos, (new_r, new_c))); downright(new_r+1,new_c+1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, new_c)))
        case _ =>
      } 
    }
    
    upleft(init_r-1, init_c-1)
    upright(init_r-1,init_c+1)
    downleft(init_r+1,init_c-1)
    downright(init_r+1,init_c+1)
    
    builder.result()
    
  }

}

case class Knight(override val color: Color) extends Piece(color) {
  def canMakeMove (move: Move): Boolean = {
    val (board, (init_r, init_c), (end_r, end_c)) = move
    // should be a knight in the initial state
    board(init_r)(init_c) == Some(Knight(color)) &&
    // should end on the board
    end_r >= 0 && end_c >= 0 &&
    end_r < 8 && end_c < 8 &&
    endPosnNotOccupied(board, (end_r, end_c)) && 
    ((end_r, end_c) == (init_r + 2, init_c + 1) ||
     (end_r, end_c) == (init_r - 2, init_c + 1) ||
     (end_r, end_c) == (init_r + 2, init_c - 1) ||
     (end_r, end_c) == (init_r - 2, init_c - 1) ||
     (end_r, end_c) == (init_r + 1, init_c + 2) ||
     (end_r, end_c) == (init_r - 1, init_c + 2) ||
     (end_r, end_c) == (init_r + 1, init_c - 2) ||
     (end_r, end_c) == (init_r - 1, init_c - 2))
  }

  def allCanMakeMoves (board: Board, initPos: Position): Vector[Move] = {
    
    val (init_r, init_c) = initPos
    
    if (board(init_r)(init_c) != Some(Knight(color)))
      return Vector.empty[Move]
    
    val builder = Vector.newBuilder[Move]
    
    val places: Vector[(Int,Int)] = Vector((1,2),(-1,2),(1,-2),(-1,-2),
    									   (2,1),(-2,1),(2,-1),(-2,-1))
    									   
    for ((delta_r, delta_c) <- places) {
      val (new_r,new_c) = (init_r + delta_r, init_c + delta_c)
      if (((0 until 8) contains new_r) &&
          ((0 until 8) contains new_c) &&
          endPosnNotOccupied(board, (new_r,new_c))) {
         builder += ((board, initPos, (new_r, new_c)))
      }
    }
    
    builder.result()
    
  }  

}

case class Queen(override val color: Color) extends Piece(color) {
  
  def canMakeMove (move: Move): Boolean = {
    val (board, (init_r, init_c), (end_r, end_c)) = move
    // should be a queen in the initial state
    board(init_r)(init_c) == Some(Queen(color)) &&
    // should end on the board
    end_r >= 0 && end_c >= 0 &&
    end_r < 8 && end_c < 8 &&
    endPosnNotOccupied(board, (end_r, end_c)) && 
    (// check if moves like a castle:
    (if (init_r == end_r) {
      // then check all intermediate col spaces are empty
       if (end_c > init_c) {
         ((init_c + 1) until end_c) forall ((i) => board(init_r)(i) == None)
       } else if (end_c < init_c) {
         ((end_c + 1) until init_c) forall ((i) => board(init_r)(i) == None)
       } else false // returning false since row and col are both fixed
    // if col is fixed
    } else if (init_c == end_c) {
      // then check all intermediate row spaces are empty
       if (end_r > init_r) {
         ((init_r + 1) until end_r) forall ((i) => board(i)(init_c) == None)
       } else if (end_r < init_r) {
         ((end_r + 1) until init_r) forall ((i) => board(i)(init_c) == None)
       } else false
    // return false because neither row nor col is fixed			
    } else false) ||
      (if (end_r - init_r == end_c - init_c) { // check if moves like a bishop
        if (init_r < end_r) {
          // intermediate spaces are empty
          (for (i <- 1 until (end_r-init_r)) yield (init_r + i, init_c + i)) forall 
            { case (r,c) => board(r)(c) == None }
        } else if (init_r > end_r) { 
          // intermediate spaces are empty
          (for (i <- 1 until (init_r-end_r)) yield (end_r + i, end_c + i)) forall
            { case (r,c) => board(r)(c) == None }
        } else false // implies no movement
      } else if (end_r - init_r == -(end_c - init_c)) {
          if (init_r > end_r) {
            (for (i <- 1 until (init_r-end_r)) yield (end_r + i, end_c - i)) forall 
              { case (r,c) => board(r)(c) == None }
          } else if (init_r < end_r) {
            (for (i <- 1 until (end_r-init_r)) yield (init_r + i, init_c - i)) forall
              { case (r,c) => board(r)(c) == None}
          } else false
      } else false))
  }
  
  def allCanMakeMoves (board: Board, initPos: Position): Vector[Move] = {
    
    val (init_r, init_c) = initPos
    
    if (board(init_r)(init_c) != Some(Queen(color)))
      return Vector.empty[Move]
    
    val builder = Vector.newBuilder[Move]
    
    @tailrec
    def down (new_r: Int) {
      if (new_r >= 8)
        return
      board(new_r)(init_c) match {
        case None => { builder += ((board, initPos, (new_r, init_c))); down(new_r+1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, init_c)))
        case _ =>
      }
    }
    
    @tailrec
    def up (new_r: Int) {
      if (new_r < 0)
        return
      board(new_r)(init_c) match {
        case None => { builder += ((board, initPos, (new_r, init_c))); up(new_r-1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, init_c)))
        case _ =>
      }
    }
    
    @tailrec
    def left(new_c: Int) {
      if (new_c < 0)
        return
      board(init_r)(new_c) match {
        case None => { builder += ((board, initPos, (init_r, new_c))); left(new_c-1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (init_r, new_c)))
        case _ =>
      }
    }
    
    @tailrec
    def right(new_c: Int) {
      if (new_c >= 8)
        return
      board(init_r)(new_c) match {
        case None => { builder += ((board, initPos, (init_r, new_c))); right(new_c+1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (init_r, new_c)))
        case _ =>
      }
    }
    
    @tailrec
    def upleft(new_r: Int, new_c: Int) {
      if (new_r < 0 || new_c < 0)
        return
      board(new_r)(new_c) match {
        case None => { builder += ((board, initPos, (new_r, new_c))); upleft(new_r-1,new_c-1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, new_c)))
        case _ =>
      } 
    }
    
    @tailrec
    def upright(new_r: Int, new_c: Int) {
      if (new_r < 0 || new_c >= 8)
        return
      board(new_r)(new_c) match {
        case None => { builder += ((board, initPos, (new_r, new_c))); upright(new_r-1,new_c+1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, new_c)))
        case _ =>
      } 
    }
    
    @tailrec
    def downleft(new_r: Int, new_c: Int) {
      if (new_r >= 8 || new_c < 0)
        return
      board(new_r)(new_c) match {
        case None => { builder += ((board, initPos, (new_r, new_c))); downleft(new_r+1,new_c-1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, new_c)))
        case _ =>
      } 
    }
    
    @tailrec
    def downright(new_r: Int, new_c: Int) {
      if (new_r >= 8 || new_c >= 8)
        return
      board(new_r)(new_c) match {
        case None => { builder += ((board, initPos, (new_r, new_c))); downright(new_r+1,new_c+1) }
        case Some(piece) if piece.color != this.color => builder += ((board, initPos, (new_r, new_c)))
        case _ =>
      } 
    }
    
    up(init_r-1)
    down(init_r+1)
    left(init_c-1)
    right(init_c+1)
    
    upleft(init_r-1, init_c-1)
    upright(init_r-1,init_c+1)
    downleft(init_r+1,init_c-1)
    downright(init_r+1,init_c+1)
    
    builder.result()
    
  }
		  	
}