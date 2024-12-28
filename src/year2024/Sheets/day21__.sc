import grid.MapGrid
import coord.Dir

val s = "029A\n980A\n179A\n456A\n379A"
val nums = s.split("\n")
nums.map(_.init.toLong)

val NumPad = MapGrid("789\n456\n123\n 0A")
val DirPad = MapGrid(" ^A\n<v>")

import Dir.*

extension (d: Dir) {
  def toDirChar: Char = {
    d match {
      case E => '>'
      case W => '<'
      case N => '^'
      case S => 'v'
      case _ => '.'
    }
  }
}

val res = NumPad.iteratorAll
  .filterNot(_._2 == ' ')
  .flatMap{(p,ch) =>
    Dir.axisDirs.flatMap{ d =>
      val newP = p.toDir(d)
      if NumPad.contains(newP) && NumPad(newP) != ' '
      then List((ch, NumPad(newP), d.toDirChar))
      else List.empty
    }
}.toList

import coord.Pos
import Pos.*

val from = 'A'
val fromPos = NumPad.posOf(from).get
val to = '8'
val toPos = NumPad.posOf(to).get
var cur = fromPos
val ds = (fromPos +-> toPos)
val ps = ds.map(d => cur.toDir(d))
//def paths(from: Char, to: Char): Seq[List[Char]] =
//  def helper(cur: Char, acc: )