import AoCLib.coord.{Coord, Pos}
import AoCLib.coord.{*, given }

import math.Ordering.Implicits.infixOrderingOps

val p1 = Pos(1,2)
val p2 = Pos(2,5)
val p3 = Pos(1,4)
val p4 = Pos(2,4)

p1 <= p2

p1 min p2
p1 min p3
p2 min p3
p2 min p4

Seq(p1, p2, p3, p4).max

val p5 = Coord.of[Pos]("3,2")

val p6 = Coord.of[Pos3D]("3,2,4")

val p7 = Pos(7,10)
val p0 = Pos(0,0)


val s1 = Set(p0,p1,p2,p3,p4,p5,p7)
Pos.render2d(s1)

val b1 = Coord.boundingBox(s1)

Pos(-1,4).in(b1)
Pos(3,3).in(b1)

PosHex.zero

p3.near4.toList
p3.near.toList
p3.all9.toList