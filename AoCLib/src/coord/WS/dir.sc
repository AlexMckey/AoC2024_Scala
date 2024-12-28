import coord.{pos, Pos}

type Dir = Pos

object Dir {
  def apply(p: Pos): Dir = Pos(p.x.sign, p.y.sign)

  def apply(x: Int, y: Int): Dir = Pos(x.sign, y.sign)

  //def unapply(d: Dir): Pos = Pos(d.x, d.y)
  val E: Dir = Pos(1, 0)
}

import Dir.*

val d1 = E
val d2 = Dir(0,1)
val p = Pos(0,-1)
d1 + p
p - d2
val d3 = Dir(Pos(3,-4))

import coord.Dir
import Dir.*

val p1 = Pos(1,-1)
val p2 = Pos(1,0)
val p3 = Pos(1,0)
val p4 = Pos(-1,0)
val p5 = Pos(5,3)
val p6 = Pos(5,4)
val p7 = Pos(4,4)

p1.asDir
p2.asDir
p1.between(p2)
p2.between(p3)
p2.between(p4)
p5.between(p6)
p6.between(p7)
import coord.GridDir.toGridChar
p6.between(p7).toGridChar
p5.between(p6).toGridChar