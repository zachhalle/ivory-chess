package gui

import javax.swing.ImageIcon
import game._

object ImageReference {
  
  private val whitePawn = new ImageIcon("images/white/pawn.png")
  private val whiteRook = new ImageIcon("images/white/rook.png")
  private val whiteKnight = new ImageIcon("images/white/knight.png")
  private val whiteBishop = new ImageIcon("images/white/bishop.png")
  private val whiteQueen = new ImageIcon("images/white/queen.png")
  private val whiteKing = new ImageIcon("images/white/king.png")
  
  private val blackPawn = new ImageIcon("images/black/pawn.png")
  private val blackRook = new ImageIcon("images/black/rook.png")
  private val blackKnight = new ImageIcon("images/black/knight.png")
  private val blackBishop = new ImageIcon("images/black/bishop.png")
  private val blackQueen = new ImageIcon("images/black/queen.png")
  private val blackKing = new ImageIcon("images/black/king.png")
  
  def apply(piece: Piece) = piece match {
  
    case Pawn(White,_) => whitePawn
    case Rook(White,_) => whiteRook
    case Knight(White) => whiteKnight
    case Bishop(White) => whiteBishop
    case Queen(White) => whiteQueen
    case King(White,_) => whiteKing
  
    case Pawn(Black,_) => blackPawn
    case Rook(Black,_) => blackRook
    case Knight(Black) => blackKnight
    case Bishop(Black) => blackBishop
    case Queen(Black) => blackQueen
    case King(Black,_) => blackKing
    
  }
  
}