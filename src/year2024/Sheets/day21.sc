import coord.{Coord, Dir, GridDir, Neighbors, Pos, pos}
import coord.GridDir.asGridDir
import graph.traverse.{BFS, DFS, Dijkstra}
import grid.VectorGrid
import grid.MapGrid

val s = "029A\n980A\n179A\n456A\n379A"
val ncs = s.split("\n").map(_.toCharArray)
val n0 = ncs(0)

val NK = MapGrid("789\n456\n123\n 0A")
val DK = MapGrid(" ^A\n<v>")

case class State(Dposs: List[Pos], Npos: Pos, code: String):
  def numPress(button: Char): Option[State] = button match
    case 'A' =>
      val btn = NK(Npos)
      Some(copy(code = code + btn))
    case _ =>
      val dir = button.asGridDir
      val npos = Npos.toDir(dir)
      if (NK.contains(npos) && NK(npos) != ' ')
        Some(copy(Npos = npos))
      else
        None // out of keypad

  def dirPress(button: Char): Option[State] = Dposs match
    case Nil => numPress(button)
    case directionalPos :: newDirectionalPoss =>
      button match
        case 'A' =>
          val newButton = DK(directionalPos)
          copy(Dposs = newDirectionalPoss).dirPress(newButton).map(newState =>
            newState.copy(Dposs = directionalPos :: newState.Dposs)
          )
        case _ =>
          val dir = button.asGridDir
          val newDirectionalPos = directionalPos.toDir(dir)
          if (DK.contains(newDirectionalPos) && DK(newDirectionalPos) != ' ')
            Some(copy(Dposs = newDirectionalPos :: newDirectionalPoss))
          else
            None // out of keypad

  def userPress(button: Char): Option[State] = dirPress(button)

import Neighbors.axisNeighbors

NK.vTree(_ == ' ').(NK.posOf('A').get, _ == NK.posOf('9').get)