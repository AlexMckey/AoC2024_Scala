import coord.{pos, Pos}

type Dir = Pos

object Dir:
  def apply(p: Pos): Dir = Pos(p.x.sign, p.y.sign)
  def apply(x: Int, y: Int): Dir = Pos(x.sign, y.sign)
  //def unapply(d: Dir): Pos = Pos(d.x, d.y)
  val E: Dir = Pos(1,0)

import Dir.*

val d1 = E
val d2 = Dir(0,1)
val p = Pos(0,-1)
d1 + p
p - d2
val d3 = Dir(Pos(3,-4))