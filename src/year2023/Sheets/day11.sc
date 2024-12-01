import AoCLib.*
import AoCLib.exts.*
import AoCLib.pos.{Pos, given}

val s = "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."

val m: Grid[Char] = s.asStrs.map(_.toVector).toVector

val mt = m.transpose

val exr = m.zipWithIndex.collect{ case (row,i) if row.forall(_ == '.') => i }
val exc = mt.zipWithIndex.collect{ case (col,i) if col.forall(_ == '.') => i }

val gs = m.zipWithIndex.flatMap((r,y) => r.zipWithIndex.collect{ case (c,x) if c == '#' => Pos(x,y) }).zipWithIndex.map((pos, i) => (i + 1) -> pos).toMap

def dist(g1: Int, g2: Int, k: Int = 1): Long =
  gs(g1).manhattanDistance(gs(g2)).toLong +
    exr.count(y => y > (gs(g1) min gs(g2)).y && y < (gs(g1) max gs(g2)).y) * k +
    exc.count(x => x > (gs(g1) min gs(g2)).x && x < (gs(g1) max gs(g2)).x) * k

gs(5).manhattanDistance(gs(9)) + exr.count(y => y > gs(5).y && y < gs(9).y) + exc.count(x => x > gs(5).x && x < gs(9).x)
dist(5,9)
dist(1,7)
dist(7,1)
dist(3,6)
dist(8,9)

val allGpairs = gs.keySet.toSeq.sorted.combinations(2).toList

val res = allGpairs.map(seq => (seq.head -> seq.last) -> dist(seq.head, seq.last))
res.map(_._2).sum

val res1 = allGpairs.map(seq => (seq.head -> seq.last) -> dist(seq.head, seq.last, 10))
res1.map(_._2).sum

val res2 = allGpairs.map(seq => (seq.head -> seq.last) -> dist(seq.head, seq.last, 100))
res2.map(_._2).sum

val m1 = s.asStrs.zipWithIndex.flatMap((row,y) =>
  row.zipWithIndex.collect{ case (c,x) if c == '#' => Pos(x,y) })
val mim1 = m1.reduce(_ min _)
val mam1 = m1.reduce(_ max _)

for
  i <- mim1.x to mam1.x
  if !m1.exists(_.x == i)
yield i

for
  i <- mim1.y to mam1.y
  if !m1.exists(_.y == i)
yield i

def toExpand(m: Seq[Pos], f: Pos => Int): Seq[Int] =
  for
    i <- f(mim1) to f(mam1)
    if !m.exists(p => f(p) == i)
  yield i

val rowExpand = toExpand(m1, _.y)
val colExpand = toExpand(m1, _.x)

import AoCLib.line.Line

def dist_(g1: Pos, g2: Pos, expR: Seq[Int], expC: Seq[Int], k: Int = 1): Long =
  val l = Line(g1, g2)
  val cnt = if k == 1 then 1 else k - 1
  val md = g1.manhattanDistance(g2).toLong
  val rs = expR.count(l.rangeByCoord(_.y).contains) * cnt
  val cs = expC.count(l.rangeByCoord(_.x).contains) * cnt
  md + cs + rs

val gs1 = m1.sorted.zipWithIndex.map((g,i) => (i+1) -> g).toMap

dist_(gs1(5),gs1(9),rowExpand,colExpand)
dist_(gs1(1),gs1(7),rowExpand,colExpand)
dist_(gs1(7),gs1(1),rowExpand,colExpand)
dist_(gs1(3),gs1(6),rowExpand,colExpand)
dist_(gs1(8),gs1(9),rowExpand,colExpand)

m1.combinations(2).map(g => dist_(g.head,g.last,rowExpand,colExpand)).sum
m1.combinations(2).map(g => dist_(g.head,g.last,rowExpand,colExpand,10)).sum
m1.combinations(2).map(g => dist_(g.head,g.last,rowExpand,colExpand,100)).sum