import grid.{CharGrid, MapGrid}
import coord.{Dir, Pos, pos}
import memo.{Cache, Memoize, Memoized}
import exts.iterables.sliding2

val s = "029A\n980A\n179A\n456A\n379A"
val nums = s.split("\n")
val compl = nums.map(_.init.toLong)

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

import graph.traverse.DFS

def ns(p: Pos): Seq[Pos] =
  p.nearAxis
    .filter(NumPad.contains)
    .filterNot(NumPad(_) == ' ')
    .toSeq

import coord.GridDir.asDirChar

val paths = DFS.search(fromPos,toPos)(ns)
val minSize = paths.map(_.size).min
val minPaths = paths.filter(_.size == minSize)
minPaths.map(_.map(NumPad(_)))
minPaths
  .map(ar => ar.sliding(2)
    .map{ case Seq(a,b) => a.between(b).asDirChar}
    .toList)

def solve(depth: Int, code: String): Long =
  given Cache[(Int, List[Char]), Long] = Cache.empty

  def presses(path: List[Pos]): List[Char] =
    //println(s"path: $path")
    if path.length < 2 then List('A')
    else
      path.sliding2
        .map(between)
        .map(_.asDirChar)
        .toList.appended('A')

  extension (g: CharGrid)
    def shortestPaths(from: Pos, to: Pos): Iterator[List[Pos]] =
      def neighbors(p: Pos): Seq[Pos] =
        p.nearAxis
          .filter(g.contains)
          .filterNot(g(_) == ' ')
          .toSeq
      val paths = DFS.search(from, to)(neighbors)
      val minSize = paths.map (_.size).min
      paths/*.reverse*/.filter(_.size == minSize).map(_.toList).iterator

  def pressCount(stage: Int, code: List[Char]): Memoized[(Int, List[Char]), Long] =
    val pad = if stage == 0 then NumPad else DirPad
    if stage == depth then code.size
    else
      ('A' :: code)
        .map(pad.posOf(_).get)
        .sliding(2)
        .map{ ar =>
          pad
            .shortestPaths(ar(0), ar(1))
            .map{lc => 
              val res = presses(lc)
//              println(res.mkString)
//              println(stage)
              res}
            .map(code => Memoize(stage, code)(pressCount(stage + 1, code)))
            .min}
        .sum

  pressCount(0, code.toCharArray.toList)

val res1 = solve(3,nums.head)
res1 * compl.head