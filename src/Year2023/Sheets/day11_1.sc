import AoCLib.exts.*
import AoCLib.coord.{ Pos, given }

val input = "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."

val s = input.asStrs
val (maxP, gs) = Pos(s.head.length-1, s.length-1) ->
  (for
    (row, y) <- s.zipWithIndex
    (c, x) <- row.zipWithIndex
    if c == '#'
  yield Pos(x,y))

val expandSpace = 1

val rowsG = (0 to maxP.y).foldLeft(Seq.empty[Pos] -> 0){ case ((acc, idx), i) =>
  val grs = gs.filter(_.y == i)
  if grs.isEmpty
  then acc -> (idx + expandSpace + 1)
  else (acc ++ grs.map(_.copy(y = idx))) -> (idx + 1)}._1

val expandedGalaxies = (0 to maxP.x).foldLeft(Seq.empty[Pos] -> 0){ case ((acc, idx), i) =>
  val grs = rowsG.filter(_.x == i)
  if grs.isEmpty
  then acc -> (idx + expandSpace + 1)
  else (acc ++ grs.map(_.copy(x = idx))) -> (idx + 1)}._1

expandedGalaxies.combinations(2)
 .map(g => g.head manhattan g.last)
 .sum

val rows = (0 to maxP.y).scanLeft(0){ case (idx, i) =>
  if gs.exists(_.y == i)
  then idx + 1
  else idx + expandSpace + 1}
val newgys = gs.map(p => Pos(p.x,rows(p.y)))

val cols = (0 to maxP.x).scanLeft(0){ case (idx, i) =>
  if gs.exists(_.x == i)
  then idx + 1
  else idx + expandSpace + 1}

val coords = (0 to (maxP.x max maxP.y)).scanLeft((0,0) -> (0,0)){ case (idx, i) =>
  val x  = if gs.exists(_.x == i)
           then idx._2._1 + 1
           else idx._2._1 + expandSpace + 1
  val y  = if gs.exists(_.y == i)
           then idx._2._2 + 1
           else idx._2._2 + expandSpace + 1
  (i,i) -> (x,y)
}
val newgs = newgys.map(p => Pos(cols(p.x),p.y))