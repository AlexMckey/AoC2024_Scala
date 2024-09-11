import AoCLib.{ Default, charDefault }
import AoCLib.coord.{ Pos, nearNeighbors }
import AoCLib.grid.{Grid, MapGrid}
import AoCLib.grid.Direction.{Column, Row}
import AoCLib.exts.*

val s = "123\n021\n412\n531"

val g: Grid[Char] = MapGrid(s)

val p1 = Pos(1,1)
val p2 = Pos(5,1)

g(p1)
g(p2)

g.gridBox

g.get(p1)
g.get(p2)

g.getOrElse(p1,'3')
g.getOrElse(p2,'3')

val gi = g.map(_.asDigit)
gi.sum

g.mapWithPos{case (Pos(y,x),v) => if x == 0 || y == 0 then '6' else v}

g.updated(p1,'5')
val ge = g.updated(p2,'5')

ge.values

ge.count(_ == '2')

ge.iterator.toList.size
ge.iteratorAll.toList.size

ge.find('0')
ge.find('2')
ge.find('5')

g.contains(p1)
g.contains(p2)
ge.contains(p2)

g.filter(_ == '2')
ge.filter(_ == '5')

g.row(1)
g.row(5)
ge.row(1)

g.rows.toList
ge.rows.toList

g.col(2)
ge.col(3)
ge.col(7)
ge.col(5)

ge.cols.toList

ge.remove(1, Row)
ge.remove(0, Column)

ge.clear(1, Row)
ge.clear(2, Column)

ge.transpose
ge.rotateCW()

ge.filter(_ == '5')

ge.filter((p,v) => p.x * p.y / 2 == 0 && v != '2')

val gd = g + (p2, '5')

ge.contains(p2)
gd.contains(p2)

ge.row(1)
gd.row(1)

ge.rows.toList
gd.rows.toList

ge.col(3)
gd.col(3)
ge.col(5)
gd.col(5)

gd.cols.toList

gd.remove(1, Row)
gd.remove(3, Column)

gd.clear(1, Row)
gd.clear(2, Column)

gd.transpose

gd -- Set(p1, p2)

val ga = gd + (Pos(3,3),'7') + (Pos(3,2),'1')

import AoCLib.coord.pos

val p3 = Pos(2,2)
p3.nearAll.toList.map(ga.get.andThen(_.map(_ == '1'))).count(_.getOrElse(false))
p3.nearAll.toList.map(ga.get)
  .count {
    case Some(x) => x == '1'
    case _       => false
  }

ga.neighborCount(Pos(2,2), _ == '1')

gd.values
gd.toVectorGrid.values
