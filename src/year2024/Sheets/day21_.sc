import coord.{Dir, Pos}
import grid.{CharGrid, MapGrid}
import memo.{Cache, Memoize, Memoized}
import exts.either
import scalaz.Monoid
import scalaz.std.vector.*
import scalaz.syntax.foldable.*
import scalaz.syntax.functor.*

import scala.annotation.targetName
import scala.collection.mutable
import scala.math.E
import scala.math.Ordering.Implicits.infixOrderingOps
import Dir.*

val NumPad = MapGrid("789\n456\n123\n 0A")
val DirPad = MapGrid(" ^A\n<v>")
val DirKeys = //Dir.axisDirs.zip("v>^<").toMap
  Map(E -> '>', W -> '<', N -> '^', S -> 'v')

val p1 = NumPad.posOf('A').get
val p2 = NumPad.posOf('7').get

p1 +-> p2
p2 +-> p1

def solve(lines: Vector[String], robots: Int): Long =
 //val cache = mutable.Map.empty[(Loc, Loc, Int), Long]
 given Cache[(Pos, Pos, Int), Long] = Cache.empty

  def bfsImpl[A, B, C](a: A, z: C, append: (C, => B) => C)(f: A => Either[Iterable[A], B]): C =
    var result = z
    val queue = mutable.Queue(a)
    while queue.nonEmpty do
      f(queue.dequeue()) match
        case Right(r) => result = append(result, r)
        case Left(states) => queue.enqueueAll(states)
    result

  def bfsMap[A, B](a: A)(f: A => Either[Iterable[A], B]): Vector[B] =
    bfsImpl[A, B, mutable.ArrayBuffer[B]](a, mutable.ArrayBuffer.empty[B], _ += _)(f).toVector

  def bfsFoldl[A, B](a: A)(f: A => Either[Iterable[A], B])(using M: Monoid[B]): B =
    bfsImpl(a, M.zero, M.append)(f)

  extension [A](v: Vector[A])
    def sliding2: Vector[(A, A)] = v.sliding(2).map(s => (s(0), s(1))).toVector

  extension [A](self: Iterator[A])
    def foldMap[B: Numeric](f: A => B): B = self.map(f).sum

  //def shortestMove(src: Loc, dst: Loc, stage: Int): Long = cache.memo((src, dst, stage)):
  def shortestMove(src: Pos, dst: Pos, stage: Int): Memoized[(Pos, Pos, Int), Long] =
    Memoize(src, dst, stage):
      given Monoid[Long] = Monoid.instance(_ min _, Long.MaxValue)
      val pad = if stage == 0 then NumPad else DirPad
      println(s"${pad(src)} -> ${pad(dst)} #$stage")
      bfsFoldl((src, Vector.empty[Char])): (loc, keys) =>
        (loc == dst).either(
          if stage < robots then shortedSolution(keys :+ 'A', stage + 1) else
            println(s"${keys.mkString}A")
            keys.length + 1L,
          (loc +-> dst).filterNot(dir => pad.isEq(loc.toDir(dir), ' '))
            .map(dir => (loc.toDir(dir), keys :+ DirKeys(dir)))
        )

  def shortedSolution(sequence: Vector[Char], stage: Int): Long =
    val pad = if stage == 0 then NumPad else DirPad
    ('A' +: sequence).map(pad.posOf(_).get).sliding2.iterator.foldMap:
      case (src, dst) => shortestMove(src, dst, stage)

  lines.iterator.foldMap: line =>
    shortedSolution(line.toVector, 0) * line.init.toLong

val s = "029A\n980A\n179A\n456A\n379A".split("\n").toVector
//val s = "286A\n974A\n189A\n802A\n805A".split("\n").toVector

solve(s,25)

solve(Vector(s(0)), 2)

def step(src: Char, dst: Char, pad: CharGrid): String =
  val (Pos(ax, ay), Pos(bx, by)) = (pad.posOf(src).get, pad.posOf(dst).get)
  val (dx, dy)                       = (bx - ax, by - ay)
  val (v, h)                         = ("v" * dy + "^" * -dy, ">" * dx + "<" * -dx)
  if dx > 0 && pad.contains(Pos(ax, by)) then s"$h${v}A"
  else if pad.contains(Pos(bx, ay)) then s"$v${h}A"
  else s"$v${h}A"

step('<', '^', DirPad)

//val robots = 2


def sol(lines: Vector[String], robots: Int): Long =
//  given Cache[(Char, Char, Int), Long] = Cache.empty
//
//  def shortMove(src: Char, dst: Char, stage: Int): Memoized[(Char, Char, Int), Long] =
//    println(s"$src -> $dst #$stage")
//    Memoize(src, dst, stage):
//      val pad = if stage == 0 then NumPad else DirPad
//      val keys = step(src, dst, pad)
//      println(keys)
//      if stage < robots
//      then shortSol(keys, stage + 1)
//      else keys.length// + 1L

  def shortSol(sequence: String, stage: Int = 0): String =
    val pad = if stage == 0 then NumPad else DirPad
    val res = ('A' + sequence).sliding(2).map(a => step(a(0), a(1), pad)).mkString
    println(res)
    if stage < robots
    then shortSol(res, stage + 1)
    else res

  lines
    .map(line => shortSol(line).length *
                 line.init.toLong)
    .sum

('A' +: "029A").sliding(2).map(a => a(0) -> a(1)).toList

sol(s, 2)
solve(s, 2)

sol(Vector(s(4)), 2)
solve(Vector(s(4)), 2)
solve(Vector("029A"), 2)
sol(Vector("029A"),2)