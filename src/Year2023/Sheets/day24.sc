import exts.*
import puzzle.Puzzle
import coord.{Pos3D, given}

import scala.annotation.tailrec
import scala.util.chaining.*

case class LongPos3D(x: Long, y: Long, z: Long)

case class Hail(p: LongPos3D, v: Pos3D)

//val s = "19, 13, 30 @ -2, 1, -2\n18, 19, 22 @ -1, -1, -2\n20, 25, 34 @ -2, -2, -4\n12, 31, 28 @ -1, -2, -1\n20, 19, 15 @ 1, -5, -3"
val s = "251454256616722, 382438634889004, 18645302082228 @ 43, -207, 371\n289124150762025, 364325878532733, 278169080781801 @ -73, -158, -13\n268852221227649, 10710819924145, 258969710792682 @ 41, 192, 62"

val stones = s.asStrs
              .map { case s"$ps @ $vs" =>
                val lps = ps.split(",\\s+").map(_.toLong)
                Hail(LongPos3D(lps(0), lps(1), lps(2)), Pos3D.build(vs.split(",\\s+").map(_.toInt))) }

val equations = new StringBuilder
for (i <- 0 until 3) {
  val t = "t" + i
  equations.append(t).append(" >= 0, ").append(stones(i).p.x).append(" + ").append(stones(i).v.x).append(t).append(" == x + vx ").append(t).append(", ")
  equations.append(stones(i).p.y).append(" + ").append(stones(i).v.y).append(t).append(" == y + vy ").append(t).append(", ")
  equations.append(stones(i).p.z).append(" + ").append(stones(i).v.z).append(t).append(" == z + vz ").append(t).append(", ")
}
val sendToMathematica = "Solve[{" + equations.substring(0, equations.length - 2) + "}, {x,y,z,vx,vy,vz,t0,t1,t2}]"
System.out.println(sendToMathematica)
val xval = 181540669791004L
val yval = 404991404832784L
val zval = 214854400593114L
val answer = xval + yval + zval

