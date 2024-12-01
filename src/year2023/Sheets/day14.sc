import AoCLib.dir.Dir
import AoCLib.exts.*
import AoCLib.grid.*
import AoCLib.pos.{Pos, posOrdering}
import AoCLib.box.Box
import AoCLib.matrix.Matrix
import AoCLib.exts.LoopExts.*

import scala.annotation.tailrec
import scala.util.chaining.*

val s = "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
val s2 = "OOOO.#.O..\nOO..#....#\nOO..O##..O\nO..#.OO...\n........#.\n..#....#.#\n..O..#.O.O\n..O.......\n#....###..\n#....#...."
val g = MapGrid(s)

g.maxRow
g.rows.zipWithIndex
  .map((st,r) => (g.maxRow + 1 - r) * st.count(_ == 'O'))
  .sum

val cs = g.cols.toSeq
val c0 = cs(0)
c0.replaceAll("#","%Z").split('%').map(_.sorted(Ordering.Char.reverse)).mkString.replaceAll("Z","#")
val c1 = cs(1)
c1.replaceAll("#","%Z").split('%').map(_.sorted(Ordering.Char.reverse)).mkString.replaceAll("Z","#")
val c2 = cs(2)
c2.replaceAll("#","%Z").split('%').map(_.sorted(Ordering.Char.reverse)).mkString.replaceAll("Z","#")
val nr = cs.map(_.replaceAll("#","%Z").split('%').map(_.sorted(Ordering.Char.reverse)).mkString.replaceAll("Z","#"))
  .transpose
  .map(_.mkString)
  .mkString("\n")
  .pipe(MapGrid.apply)

val northLoad = nr.rows.zipWithIndex
  .map((st,r) => (nr.maxRow + 1 - r) * st.count(_ == 'O'))
  .sum

val gO = g.findAll(1,"O".r)
gO.filter(_.x == 0)

val gD = g.findAll(1,"#".r)
given Seq[Pos] = gD.toSeq
given (Int,Int) = g.maxCol -> g.maxRow

def step(stones: Seq[Pos])(using wall: Seq[Pos], maxCoord: (Int,Int)): Seq[Pos] =
  val gd = wall ++ (0 to maxCoord._1).map(x => Pos(x, maxCoord._2 + 1))
  (0 to maxCoord._1)
    .foldLeft(Seq.empty[Pos])((allP, x) =>
      val gp = gd.filter(_.x == x).sorted
      //println(gp)
      val gs = stones.filter(_.x == x)
      //println(gs)
      val stepP = gp.foldLeft(Seq.empty[Pos], gs, 0){
        case ((acc, ss, cury), dp) =>
          //println(dp)
          val (fall, notFall) = ss.partition(p => p.y < dp.y)
          //println(fall)
          (acc ++ fall.zipWithIndex.map((p,i) => Pos(p.x, cury + i)), notFall, dp.y + 1)
      }._1
      //println(stepP)
      allP ++ stepP)
step(gO.toSeq).sortBy(p => p.y -> p.x)
val step1 = MapGrid(s2).findAll(1,"O".r).toSeq.sortBy(p => p.y -> p.x)
val gr = MapGrid(g.cols.map(_.reverse).mkString("\n"))

//@tailrec
//def fall(toFall: Set[Pos], wall: Set[Pos], d: Dir, stones: Set[Pos]): Set[Pos] =
//  //println(s"s: $stones")
//  if toFall.isEmpty then stones
//  else
//    val wallStopped = toFall.filter(p =>
//      val newP = p.toDir(d)
//      wall.contains(newP) || !g.gridBox.contains(newP)
//    )
//    //println(s"w: $wallStopped")
//    val other = toFall -- wallStopped
//    val stoneStopped = other.filter(p => toFall.contains(p.toDir(d)))
//    //println(s"ss: $stoneStopped")
//    //val falling = (other -- stoneStopped).map(_.toDir(d))
//    //println(s"f: $falling")
//    fall(falling ++ stoneStopped, wall ++ wallStopped, d, stones ++ wallStopped)
////    if falling.isEmpty
////    then fall(falling, wall ++ wallStopped, d, stones ++ wallStopped ++ stoneStopped)
////    else fall(falling ++ stoneStopped, wall ++ wallStopped, d, stones ++ wallStopped)

@tailrec
def fall_(toFall: Set[Pos], wall: Set[Pos], d: Dir, stones: Set[Pos]): Set[Pos] =
  val gn = MapGrid((toFall.map(_ -> 'O') ++ wall.map(_ -> '#')).toMap)
  println("\n"+gn.toString+"\n")
  if toFall.isEmpty then stones
  else
    val falling = toFall.map(_.toDir(d))
    val notFalling = falling intersect wall ++ falling.filterNot(g.gridBox.contains)
    val wallStopped = notFalling.map(_.toDir(d.opposite))
    val stoneStopped = (toFall intersect falling).map(_.toDir(d.opposite))
    //val stoneStopped = (toFall intersect wallStopped).map(_.toDir(d.opposite))
    val other = falling -- wallStopped -- notFalling  -- stoneStopped.map(_.toDir(d))
    fall_(stoneStopped ++ other, wall ++ wallStopped, d, stones ++ wallStopped)


//val res1 = fall(gO, gD, Dir.N, Set.empty).toSeq.sortBy(p => p.x -> p.y)
//step1.toSeq.sortBy(p => p.x -> p.y)

var wall = gD
var toFall = gO
wall.size + toFall.size
var falling = toFall.map(_.toDir(Dir.N))
var notFalling = falling intersect wall ++ falling.filterNot(g.gridBox.contains)
var wallStopped = notFalling.map(_.toDir(Dir.N.opposite))
//var stoneStopped = toFall intersect wallStopped.map(_.toDir(Dir.N.opposite))
var stoneStopped = (toFall intersect falling).map(_.toDir(Dir.N.opposite))
var other = falling -- wallStopped -- notFalling -- stoneStopped.map(_.toDir(Dir.N))
wallStopped.size + other.size + stoneStopped.size
wall ++= wallStopped
toFall = stoneStopped ++ other
wall.size + toFall.size

falling = toFall.map(_.toDir(Dir.N))
notFalling = falling intersect wall ++ falling.filterNot(g.gridBox.contains)
wallStopped = notFalling.map(_.toDir(Dir.N.opposite))
//stoneStopped = toFall intersect wallStopped.map(_.toDir(Dir.N.opposite))
stoneStopped = (toFall intersect falling).map(_.toDir(Dir.N.opposite))
other = falling -- wallStopped -- notFalling
wallStopped.size + other.size + stoneStopped.size
wall ++= wallStopped
toFall = stoneStopped ++ other
wall.size + toFall.size

val sn = fall_(gO,gD,Dir.N,Set.empty)
val gn = MapGrid((sn.map(_ -> 'O') ++ gD.map(_ -> '#')).toMap)
"\n"+gn.toString
val sw = fall_(sn,gD,Dir.W,Set.empty)
val gw = MapGrid((sw.map(_ -> 'O') ++ gD.map(_ -> '#')).toMap)
"\n"+gw.toString
val sc = Set(Dir.N, Dir.W, Dir.S, Dir.E).foldLeft(gO)((st, d) => fall_(st,gD,d,Set.empty))
val gc = MapGrid((sc.map(_ -> 'O') ++ gD.map(_ -> '#')).toMap)
"\n"+gc.toString

wall = gD
toFall = sn
falling = toFall.map(_.toDir(Dir.W))
notFalling = falling intersect wall ++ falling.filterNot(g.gridBox.contains)
wallStopped = notFalling.map(_.toDir(Dir.W.opposite))
//stoneStopped = toFall intersect wallStopped.map(_.toDir(Dir.N.opposite))
stoneStopped = (toFall intersect falling).map(_.toDir(Dir.W.opposite))
other = falling -- wallStopped -- notFalling
wallStopped.size + other.size + stoneStopped.size
wall ++= wallStopped
toFall = stoneStopped ++ other
wall.size + toFall.size

def prep(input: String) =
  val ss = input.asStrs
  val box = Box(Pos.zero, Pos(ss.head.length, ss.length))
  val maps = ss.zipWithIndex
               .flatMap((row,y) => row.zipWithIndex
                                      .collect{ case (c,x) if c != '.' => Pos(x,y) -> c })
  box -> maps.toSet
             .partitionMap{
               case (p, 'O') => Left(p)
               case (p, '#') => Right(p)
             }

@tailrec
def fall(toFall: Set[Pos], wall: Set[Pos], d: Dir, stones: Set[Pos] = Set.empty)(using gridBox: Box): Set[Pos] =
  if toFall.isEmpty then stones
  else
    val falling      = toFall.map(_.toDir(d))
    val notFalling   = falling intersect wall ++ falling.filterNot(gridBox.contains)
    val wallStopped  = notFalling.map(_.toDir(d.opposite))
    val stoneStopped = (toFall intersect falling).map(_.toDir(d.opposite))
    val other        = falling -- wallStopped -- notFalling -- stoneStopped.map(_.toDir(d))
    fall(stoneStopped ++ other, wall ++ wallStopped, d, stones ++ wallStopped)

def cycleTilt(stones: Set[Pos])(using walls: Set[Pos], gridBox: Box): Set[Pos] =
  Set(Dir.N, Dir.W, Dir.S, Dir.E).foldLeft(stones)((st, d) => fall(st, walls, d))

val (b, (st, w)) = prep(s)
given Box = b
given Set[Pos] = w
w == gD
st == gO

val r0 = fall(st,w,Dir.N)
val g0 = MapGrid((r0.map(_ -> 'O') ++ w.map(_ -> '#')).toMap)
"\n"+g0.toString

val r0_1 = fall(r0,w,Dir.W)
val g0_1 = MapGrid((r0_1.map(_ -> 'O') ++ w.map(_ -> '#')).toMap)
"\n"+g0_1.toString

val r0_2 = fall(r0_1,w,Dir.S)
val g0_2 = MapGrid((r0_2.map(_ -> 'O') ++ w.map(_ -> '#')).toMap)
"\n"+g0_2.toString

val r0_3 = fall(r0_2,w,Dir.E)
val g0_3 = MapGrid((r0_3.map(_ -> 'O') ++ w.map(_ -> '#')).toMap)
"\n"+g0_3.toString

val r1 = cycleTilt(gO)
val g1 = MapGrid((r1.map(_ -> 'O') ++ w.map(_ -> '#')).toMap)
"\n"+g1.toString

val rg = "(#*)?([O.]+)(#*)?".r
val sorts = "O."

def superFall(s: String): String =
  rg.findAllMatchIn(s)
    .map(_.subgroups match
      case List(f, snd, e) => s"$f${snd.sorted(Ordering.by[Char, Int](sorts.indexOf(_)))}$e"
      case l => l.toString)
    .mkString

extension (g: MapGrid)
  def tilt: MapGrid =
    g.cols.map(superFall).mkString("\n").pipe(MapGrid.apply).transpose

val t1 = Matrix.rotateCW[Int]
val t2 = Matrix.rotateCCW[Int]

val nr0 = g.cols.map(superFall).mkString("\n").pipe(MapGrid.apply).transpose
var nr1 = nr0.rows.map(superFall).mkString("\n").pipe(MapGrid.apply)
var nr2 = nr1.cols.map(_.reverse).map(superFall).map(_.reverse).mkString("\n").pipe(MapGrid.apply).transpose
var nr3 = nr2.rows.map(_.reverse).map(superFall).map(_.reverse).mkString("\n").pipe(MapGrid.apply)

g
  //.transform(t1)
  .tilt
  .transform(t2)
  .tilt
  .transform(t2)
  .tilt
  .transform(t2)
  .tilt
  .transform(t2)
  //.transform(t2)

4.repeated(g)(_.tilt.transform(t2))