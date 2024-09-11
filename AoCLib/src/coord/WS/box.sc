import AoCLib.box.Box
import AoCLib.coord.{Pos, given}
import AoCLib.coord.Dir.*
import math.Ordering.Implicits.infixOrderingOps
import AoCLib.exts.Iterables.*

val bx = Box(Pos(0,0), Pos(139, 139))
val p = Pos(86,0)
bx.contains(p)
val p1 = p.toDir(S)
bx.contains(p.toDir(S))
val dl = bx.dl
val ur = bx.ur
dl <= p && p <= ur
dl <= p1 && p1 <= ur

dl <= p1
dl.y
p1.y
dl.y.compare(p1.y)
dl.y <= p1.y

val a1 = Seq(0,0)
val a2 = Seq(86,-1)

a1.zip(a2)
  .map(_.compare(_)).collectFirst { case x if x != 0 => x}
  .getOrElse(0)

a1.zip(a2)
  .map(_.compare(_))
  .groupCount(identity) match {
  case m if m.contains(-1) => -1
  case m if m.contains(1) => 1
  case _ => 0
  }
import math.Ordering.Implicits.seqOrdering
a1 <= a2
import math.Ordered.orderingToOrdered
a1.compare(a2)

1.compare(-1)
(-1).compare(1)
