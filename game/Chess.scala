package game

// en passant, castling, pawn promotion does not work
		
object Chess extends Game {
  
  type Board = Vector[Vector[Option[Piece]]]
  //(row,column)
  type Position = (Int,Int)
  //(current board, turn number, player to move)
  type State = (Board, Int, Player)
  //board, init position, final position
  type Move = (Board, Position, Position)
  
  var lastTakenPieceTurn = 0
  
  //sequence of all possible moves for player to move
  //moves illegal due to check should be excluded
  override def moves (s: State): Vector[Move] = {
     val pieces = piecesWithPositions(player(s), board(s))
     (pieces map { case (piece, position) => piece.allMovesFrom(board(s), position) }).flatten
  }
  
  //determines if there is a victor or still in play
  override def status (s: State): Status = {
    
    if (lastTakenPieceTurn == 50) {
      Over(Tie())
    }
    
    //all moves possible for player to move
    val moves = this.moves(s)
   
    //no moves implies any move puts you in check
    if (moves isEmpty) {
    
      // player to move
      val player = this.player(s)
      
      // if every move puts you into check and currently in check,
      // checkmate
      if (currentPlayerInCheck(board(s), player)) 
          Over(Winner(flipPlayer(player)))
      // stalemate
      else 
          Over(Tie())
    
    }
    
    //there are moves which do not put the current player in check
    else InPlay()
 
  }
  
  //board
  def board (s: State) = s match {
    case (b: Board, _, _) => b
  }
  
  def brd (m: Move) = m match {
    case (b: Board, _, _) => b
  }
  
  //turn
  override def turn (s: State): Int = s match {
    case (_, t: Int, _) => t
  }
  
  //player to move
  override def player (s: State): Player = s match {
    case (_, _, p: Player) => p
  }
  
  def flipPlayer (p: Player): Player = p match {
    case Maxie => Minnie
    case Minnie => Maxie
  }
  
  //construct initial board
  def initBoard(): Board = {
    
    def endRow(c: Color): Vector[Option[Piece]] =
      Vector(Some(Rook(c, true)), Some(Knight(c)), Some(Bishop(c)), 
          Some(Queen(c)), Some(King(c, true)), Some(Bishop(c)), 
          Some(Knight(c)), Some(Rook(c, true)))
  
  
    def pawns(c: Color): Vector[Option[Piece]] = 
      Vector.tabulate(8)((i: Int) => Some(Pawn(c,true)))
    
    //empty rows
    val nothings: Vector[Option[Piece]] =
      Vector.tabulate(8)((i: Int) => None)
      
    Vector.tabulate(8)({
      case 0 => endRow(Black)
      case 1 => pawns(Black)
      case i if 2 to 5 contains i => nothings
      case 6 => pawns(White)
      case 7 => endRow(White)
    })
    
  }
  
  def start(): State = (initBoard(), 0, Maxie)
  
  def findKing(board: Board, player: Player): (Piece, Position) = {
       
    for (r <- 0 until board.length) {
      for (c <- 0 until board(r).length) {
        board(r)(c) match {
          case Some(piece: King) if piece colorMatchesPlayer player => return (piece, (r,c))
          case _ =>
         }
       }
     }  
     throw new GameException("Couldn't find " + player + " king")    
  }
  
  def currentPlayerInCheck (board: Board, player: Player): Boolean = {
    
    val (_, kingLoc) = findKing(board, player)
    val pieces = piecesWithPositions(flipPlayer(player), board, includeKing = false)
    // TODO needs to be adjusted for en passant and shit
    pieces exists { case (piece, position) => piece canMakeMove (board, position, kingLoc) }
    
  }
  
  //determines if the move is legal
  override def isLegalMove (m: Move, s: State): Boolean = {
    
    val board = this.board(s)
    val player = this.player(s)
    
    //getting the init position of the move
    val (_, (row,col), _) = m
    
    //piece option of the move
    board(row)(col) match {
     
      //no piece, so no move
      case None => false 
      
      case Some(piece) => {
        //the piece must be able to move to the desired posn
        (piece canMakeMove m) &&
        //the desired posn cannot put yourself in check
        (!currentPlayerInCheck(this.board(applyMove (m, s)), player))
      }
    }
  }
  
  
  //all the pieces of the player to move zipped with their position in the board
  def piecesWithPositions(player: Player, b: Board, includeKing: Boolean = true): Vector[(Piece, Position)] = {
    
    //zip position into board
    val boardWithPositions = Vector.tabulate(b.length, b(0).length)(
    (x,y) => 
      b(x)(y) match {
        case Some(piece) => Some(piece, (x,y))
        case None => None
      }
    )
      
    boardWithPositions.flatten.filter((p) => 
      p match {
        case Some((king: King, _)) if king colorMatchesPlayer player => includeKing
        case Some((piece, _)) if piece colorMatchesPlayer player => true
        case _ => false
      }).map((p) =>
        p match {
          case Some((piece, (x,y))) => (piece, (x,y))
        }
      )
      
  } 
  
  def pieces(player: Player, b: Board): Vector[Piece] = 
    b.flatten.filter((p) =>
      
      p match {
        case Some(piece) => true
        case None => false
      }
      
      ).map((p) => 
        p match { case Some(piece) => piece })
  
  //requires: piece.canMoveTo(board, m)
  //requires: board(m) == board(s)
  def applyMove (m: Move, s: State): State = {
    
    val (board,init,end) = m
    val (init_r, init_c) = init
    val (end_r, end_c) = end
    
    val pieceAt = board(init_r)(init_c) match {
      case Some(Pawn(color,true)) => Pawn(color,false)
      case Some(Rook(color,true)) => Rook(color,false)
      case Some(King(color,true)) => King(color,false)
      case Some(p) => p
    }
    
    val newBoard = Vector.tabulate(board.length, board(0).length)(
        // TODO this will need to be adapted to handle castling, en passant, and pawn promotion
        (x,y) =>
          if (init == (x,y))
            None
          else if (end == (x,y)) {
            
            if (board(x)(y) != None) {
              lastTakenPieceTurn = 0
            } else {
              lastTakenPieceTurn += 1
            }
            
            if (x == 0 && (pieceAt match { 
              case Pawn(White,_) => true
              case _ => false
            }))
              Some(Queen(White))
            else if (x == 7 && (pieceAt match { 
              case Pawn(Black,_) => true
              case _ => false
            }))
              Some(Queen(Black))
            else
              Some(pieceAt)
          } 
          else
            board(x)(y)
    )
    
    pieceAt match {
      case King(White,false) if end_c - init_c == 2 => {
        val row = newBoard(7)
        (newBoard.updated(7, row.updated(5, Some(Rook(White,false))).updated(7, None)), turn(s)+1, flipPlayer(player(s)))
      }
      case King(White,false) if end_c - init_c == -2 => {
        val row = newBoard(7)
        (newBoard.updated(7, row.updated(3, Some(Rook(White,false))).updated(0, None)), turn(s)+1, flipPlayer(player(s)))
      }
      case King(Black,false) if end_c - init_c == 2 => {
        val row = newBoard(0)
        (newBoard.updated(0, row.updated(5, Some(Rook(Black,false))).updated(7, None)), turn(s)+1, flipPlayer(player(s)))
      }
      case King(Black,false) if end_c - init_c == -2 => {
        val row = newBoard(0)
        (newBoard.updated(0, row.updated(3, Some(Rook(Black,false))).updated(0, None)), turn(s)+1, flipPlayer(player(s)))
      }
      case _ => (newBoard, turn(s) + 1, flipPlayer(player(s)))
    }
  
  }

  def estimate (s: State): Int = {
    
    def scoreMaterial(piece: Piece, total: Int) =
        piece match {
          case King(_,_) => total + 2000
          case Queen(_) => total + 90
          case Rook(_,_) => total + 50
          case Bishop(_) => total + 30
          case Knight(_) => total + 30
          case Pawn(_,_) => total + 10
        }
    
    val whiteMaterial = 
      pieces(Maxie, board(s)).foldRight(0)(scoreMaterial)
        
    val blackMaterial =
      pieces(Minnie, board(s)).foldRight(0)(scoreMaterial)
    
    val whiteMobility = moves((board(s), turn(s), Maxie)).length
    val blackMobility = moves((board(s), turn(s), Minnie)).length
    
    (whiteMaterial - blackMaterial) + (whiteMobility - blackMobility)
        
  }
  
}