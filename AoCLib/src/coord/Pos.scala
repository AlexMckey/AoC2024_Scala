package coord

import scala.io.AnsiColor.*

case class Pos(x: Int, y: Int) derives CanEqual:
  override def toString: String = this.show
  def rotateTimes(times: Int): Pos =
    val left = if times < 0
    then scala.math.ceil(-times / 4.0).toInt * 4 + times
    else times
    (1 to left).foldLeft(this) { case (Pos(x, y), _) => Pos(-y, x) }
  // (2, 1) - left => (-1, 2)
  def cross(that: Pos): Long = x.toLong * that.y - that.x.toLong * y
  def pair: (Int, Int) = x -> y
  def swap: Pos = Pos(y, x)
  def toDir(d: Dir): Pos = this + d.delta

object Pos:
  private def simpleRender: Pos => String = _ => s"$WHITE_B#"
  def render2d(ps: Set[Pos], render: Pos => String = simpleRender, width: Int = 1): Unit =
    if ps.nonEmpty then
      val (tl, br) = Coord.boundingBox(ps)
      println()
      for y <- tl.y to br.y do
        val line = for x <- tl.x to br.x yield
          val p = Pos(x,y)
          if ps(p) then render(p) else s"$BLUE.".padTo(width, ' ')
        println(line.mkString(" "))
    println()
  def zero: Pos = Coord.zero[Pos]
  extension (pr: (Int, Int))
    def toPos: Pos = Pos(pr._1, pr._2)

given pos: Coord[Pos] with
  type Item = Int
  extension (v: Pos)
    def axis: Seq[Int] = Seq(v.x, v.y)
  def build(xs: Seq[Item]): Pos = Pos(xs.head, xs.last)
  val size: Int = 2

//given posOrdering: Ordering[Pos] with
//  override def compare(p1: Pos, p2: Pos): Int =
//    val yy = p1.y - p2.y
//    if yy == 0 then p1.x - p2.x else yy