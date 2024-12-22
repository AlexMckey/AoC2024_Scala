import coord.{Dir, Pos}
import grid.MapGrid
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

given Cache[(Pos, Pos, Int), Long] = Cache.empty

val p1 = NumPad.posOf('A').get
val p2 = NumPad.posOf('6').get

p1 +-> p2

def solve(lines: Vector[String], robots: Int): Long =
 //val cache = mutable.Map.empty[(Loc, Loc, Int), Long]

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
      bfsFoldl((src, Vector.empty[Char])): (loc, keys) =>
        (loc == dst).either(
          if stage < robots then shortedSolution(keys :+ 'A', stage + 1) else
            println(s"$keys  ${keys.length + 1}")
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

//val s = "029A\n980A\n179A\n456A\n379A".split("\n").toVector
val s = "286A\n974A\n189A\n802A\n805A".split("\n").toVector

solve(s,25)

solve(Vector(s(0)), 2)