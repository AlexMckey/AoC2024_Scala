import common.Default
import coord.{ Pos, given }
import grid.MapGrid
val s = "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"
//given Default[Char] = '.'
val g = MapGrid(s)
val antennas = ('A' to 'Z').concat('a' to 'z').concat('0' to '9')
val ms = g.filter(antennas.contains).iterator.toList.groupMap(_._2)(_._1)
val cs = ms.map(_._2.combinations(2).toList)
val ps = cs.flatMap(_.flatMap{ case List(a,b) =>
  val d = b - a
  List(a - d, b + d)
}.filter(g.contains)).toSet
ps.size
