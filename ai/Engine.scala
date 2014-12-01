package ai

import game.Chess
import game.Chess._
import game.Settings
import scala.annotation.tailrec

object Engine {
  
  type Edge = (Move, Int)
  
  sealed abstract class Value
  case class BestEdge(edge: Edge) extends Value
  case object Pruned extends Value
  
  sealed abstract class Result
  case class Val(value: Value) extends Result
  case object ParentPrune extends Result
  
  type AlphaBeta = (Value, Value)
  
  sealed abstract class Tree
  case class Node(left: Tree, val data: Int, right: Tree) extends Tree
  case class Leaf(data: Int) extends Tree
  
  // the following are all helpers for search and evaluate functions
  private
  def alphaIsLessThan(alpha: Value, v: Int) = alpha match {
    case Pruned => true
    case BestEdge((_,alphav)) => alphav < v
  }
  
  private
  def maxAlpha(values: (Value, Value)) = values match {
    case (Pruned, y) => y
    case (x, Pruned) => x
    case (BestEdge((m1, e1)), BestEdge((m2, e2))) => 
      if (e1 < e2)
        BestEdge((m2,e2))
      else
        BestEdge((m1,e1))
  }
  
  private
  def betaIsGreaterThan(v: Int, beta: Value) = beta match {
    case Pruned => true
    case BestEdge((_, betav)) => v < betav
  }
  
  private
  def minBeta(values: (Value, Value)) = values match {
    case (Pruned, y) => y
    case (x, Pruned) => x
    case (BestEdge((m1,e1)), BestEdge((m2,e2))) =>
      if (e1 < e2)
        BestEdge((m1,e1))
      else
        BestEdge((m2,e2))
  }
  
  private
  def updateAB(state: State)(ab: AlphaBeta)(value: Value): AlphaBeta = {
    val (alpha, beta) = ab
    
    value match {
      case BestEdge((_,est)) => player(state) match {
        case Maxie => (maxAlpha((alpha,value)), beta)
        case Minnie => (alpha, minBeta(beta,value))
      }
      case Pruned => (alpha,beta)
    }
  }
  
  private
  def valueFor(state: State)(ab: AlphaBeta): Value = {
    val (alpha,beta) = ab
    player(state) match {
      case Maxie => alpha
      case Minnie => beta
    }
  }

  private
  def checkBounds(ab: AlphaBeta)(state: State)
  				 (incomingMove: Move)(v: Int) = {
    
    val (alpha,beta) = ab
    
    if (alphaIsLessThan(alpha, v)) {
      if (betaIsGreaterThan(v, beta)) {
        Val(BestEdge((incomingMove, v)))
      } else {
        player(state) match {
          case Maxie => Val(Pruned)
          case Minnie => ParentPrune
        }
      }
    } else {
      player(state) match {
        case Maxie => ParentPrune
        case Minnie => Val(Pruned)
      }
    }
  }
  
  // give the AB result of a state
  private def evaluate(depth: Int)(ab: AlphaBeta)(state: State)(lastMove: Move): Result =
    status(state) match {
      case Over(Winner(Maxie)) => Val(BestEdge((lastMove, Int.MaxValue)))
      case Over(Winner(Minnie)) => Val(BestEdge((lastMove, Int.MinValue)))
      case Over(Tie()) => checkBounds(ab)(state)(lastMove)(0)
      case InPlay() =>
        depth match {
          case 0 => checkBounds(ab)(state)(lastMove)(estimate(state))
          case _ => 
            search(depth)(ab)(state)(moves(state)) match {
      	      case BestEdge((mv, est)) => checkBounds(ab)(state)(mv)(est)
      		  case Pruned => Val(Pruned)
      	    }
        }
    }
  
  // search the children of a game state
  @tailrec
  private def search(depth: Int)(ab: AlphaBeta)(state: State)(moves: Vector[Move]): Value = 
    moves match {
      case Vector() => valueFor(state)(ab)
      case move +: tail =>
        evaluate(depth-1)(ab)(applyMove(move,state))(move) match {
          case ParentPrune => Pruned
          case Val(Pruned) => search(depth)(ab)(state)(moves.tail)   
          case Val(BestEdge((_,est))) => search(depth)( updateAB(state)(ab)(BestEdge((move,est))) )(state)(moves.tail)
      }
    }                 
  
  def computeMove(state: State): Move = {
    val move: Move = null
    evaluate(Settings.searchDepth)((Pruned, Pruned))(state)(move) match {
      case Val((BestEdge((mv, _)))) => mv
      case _ => throw new GameException("Found no move")
    }
  }
}
