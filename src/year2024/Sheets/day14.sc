import box.Box
import coord.{Pos, given}
import math.%+
import exts.maps.plusAt

case class Robot(p: Pos, v: Pos):
  def after(time: Int)(using area: Box): Robot =
    val newPos = p + v * time
    //Robot(Pos(newPos.x % area.max.x, newPos.y % area.max.y), v)
    Robot(Pos(newPos.x %+ area.max.x, newPos.y %+ area.max.y), v)

val s = "p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3"

val rs = s.split("\n").map{
  case s"p=$px,$py v=$vx,$vy" => Robot(Pos(px.toInt,py.toInt), Pos(vx.toInt, vy.toInt))
}

given b: Box = Box(Pos(0,0), Pos(11,7))
val pDiv = (b.max - Pos(1,1)) / 2

rs.groupMapReduce(_.p)(_=>1)(_+_).values

val r = Robot(Pos(2, 4), Pos(2, -3))

r.after(4).p

rs
  .map(r => r.p - pDiv)//.filterNot(p => p.x == 0 || p.y == 0)
  .foldLeft(Map.empty[Int,Int].withDefaultValue(0)) { (m,p) =>
    p match
      case p if p.x < 0 && p.y > 0 => m.plusAt(3)
      case p if p.x > 0 && p.y > 0 => m.plusAt(4)
      case p if p.x < 0 && p.y < 0 => m.plusAt(1)
      case p if p.x > 0 && p.y < 0 => m.plusAt(2)
      case _ => m.plusAt(0)
  }
val res = rs.map(_.after(100))
//  .map(_.p)
//  .sorted
  .map(r => r.p - pDiv)//.filterNot(p => p.x == 0 || p.y == 0)
  .foldLeft(Map.empty[Int,Int].withDefaultValue(0)) { (m,p) =>
    p match
      case p if p.x < 0 && p.y > 0 => m.plusAt(3)
      case p if p.x > 0 && p.y > 0 => m.plusAt(4)
      case p if p.x < 0 && p.y < 0 => m.plusAt(1)
      case p if p.x > 0 && p.y < 0 => m.plusAt(2)
      case _ => m.plusAt(0)
  }
res.filterNot(_._1 == 0).values.product

import scala.math.Ordered.orderingToOrdered
rs(0).p
pDiv
rs.map(_.p.compare(pDiv))
Ordering[Pos].compare(rs(0).p,pDiv)

extension (p: Pos)
  def quadrant(center: Pos): Pos =
    val d = p - center
    Pos(d.x.sign, d.y.sign)

import exts.iterables.groupCount
rs.map(_.after(100)).map(_.p.quadrant(pDiv))
  .groupCount(identity)
  .filterNot(_._1.multiple == 0)
  .values
  .product