import coord.{Coord, Dir, Pos, allDirs, diagDirs, given}
import Dir.*
import grid.MapGrid
import exts.repeated

val s = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
val g = MapGrid(s)
val xs = g.filter(_ == 'X').allPos.toList

xs.map(p => allDirs
    .count(d => List.fill(3){d}
      .scanLeft(p){_ + _.delta}
      .flatMap(g.get)
      .mkString == "XMAS"))
  .sum

g.findAll(4, "XMAS".r, allDirs).size

val as = g.filter(_ == 'A').allPos.toList
val xls = List("SMMS", "MMSS", "MSSM", "SSMM")

val ls = as.map(p => diagDirs
    .map(d => p + d.delta)
      .flatMap(g.get)
      .mkString)

ls.count(xls.contains)

g.findAll(3, "MAS".r, diagDirs).size