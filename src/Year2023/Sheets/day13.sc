import AoCLib.exts.*
import AoCLib.grid.MapGrid

import scala.annotation.tailrec
import scala.util.chaining.{*, given}

enum Axis(at: Int):
  case V(at: Int) extends Axis(at)
  case H(at: Int) extends Axis(at)
  def score: Int = this match
    case V(x) => 100 * x
    case H(x) => x

def prep(input: String): Seq[Seq[String]] =
  input.splitByBlankLines.map(_.asStrs)

//val s = "......#....#...\n.####...##...#.\n######..##..###\n######..##..###\n......##..##...\n.####.######.##\n.#..#.######.#."
val s = "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#"

prep(s).head
val mcs = s.splitByBlankLines.map(_.asStrs)
val m1 = mcs.head
val m2 = mcs.last
def mirror(m: Seq[String]): (Seq[(Int,Int)], Seq[(Int, Int)]) =
  val mt = m.transpose
  m.zipWithIndex.collect { case (f, i) if m.lastIndexWhere(s => s == f) != i => i -> m.lastIndexWhere(s => s == f) } ->
  mt.zipWithIndex.collect { case (f, i) if mt.lastIndexWhere(s => s == f) != i => i -> mt.lastIndexWhere(s => s == f) }

m1.zipWithIndex.collect{case (f,i) if m1.lastIndexWhere(s => s == f) != i => i -> m1.lastIndexWhere(s => s == f)}
m1.transpose.zipWithIndex.collect{case (f,i) if m1.transpose.lastIndexWhere(s => s == f) != i => i -> m1.transpose.lastIndexWhere(s => s == f)}

mirror(m1)
mirror(m2)

@tailrec
def findIndexes(m: Seq[String])(to: Int, from: Int = 0, isEq: Boolean = false): Option[Int] =
  (m(from) == m(to), from, to, isEq) match
    case (true, f, t, _) if t == f + 1 => println("0"); Some(t)
    case (true, f, t, _) => println("1"); findIndexes(m)(t - 1, f + 1, true)
    case (false, f, t, false) if t == f + 1 => println("2"); findIndexes(m)(m.length - 1, f + 1, false)
    case (false, f, t, false) => println("3"); findIndexes(m)(t - 1, f, false)
    case (false, _, _, true) => println("4"); None
    case _ => println("Q"); None

@tailrec
def findIndexes1(m: Seq[String])(to: Int, from: Int = 0, isEq: Boolean = false): Option[Int] =
  (m(from) == m(to), from, to, isEq) match
    case (true, f, t, _) if t == f + 1 => Some(t)
    case (true, f, t, _) => findIndexes1(m)(t - 1, f + 1, true)
    case (false, _, _, true) => None
    case (false, f, t, false) if t == 1 => findIndexes1(m)(m.length - 1, f + 1, false)
    case (false, f, t, false) if f == t - 1 => None
    case (false, f, _, false) if f != 0 => findIndexes1(m)(m.length - 1, f + 1, false)
    case (false, f, t, false) => findIndexes1(m)(t - 1, f, false)
    case _ => None

def findMirror(m: Seq[String]): Option[Axis] =
  val mt = m.transpose.map(_.mkString)
  (findIndexes1(m)(m.length-1), findIndexes1(mt)(mt.length-1)) match
    case (Some(v), Some(h)) if v > h => Some(Axis.V(v))
    case (Some(_), Some(h)) => Some(Axis.H(h))
    case (None, h) => h.map(Axis.H.apply)
    case (v, None) => v.map(Axis.V.apply)
    case _ => None

findIndexes(m1)(m1.length-1)
findIndexes(m1.transpose.map(_.mkString))(m1.transpose.length-1)

findIndexes(m2)(m2.length-1)
findIndexes(m2.transpose.map(_.mkString))(m2.transpose.length-1)

findIndexes1(m1)(m1.length-1)
findIndexes1(m1.transpose.map(_.mkString))(m1.transpose.length-1)

findIndexes1(m2)(m2.length-1)
findIndexes1(m2.transpose.map(_.mkString))(m2.transpose.length-1)

mcs.map(findMirror).map(_.map(_.score).getOrElse(0)).sum

def splitAndCheck(m: Seq[String], at: Int): Int =
  m.map(_.take(at * 2)
    .splitAt(at))
    .map((l,r) => l.reverse
      //.take(r.length)
      .zip(r)
      .count(_ != _))
    .sum

def findIndex(m: Seq[String]): Option[Int] =
  (1 until (m.head.length - 1))
    .map(splitAndCheck(m,_))
    .zipWithIndex
    .collectFirst { case (check, i) if check == 1 => i}

def findMirrors(m: Seq[String]): Int =
  findIndex(m).map(_ + 1).getOrElse{
    findIndex(m.transpose.map(_.mkString))
      .map(_ + 1)
      .map(_ * 100).getOrElse(0)}


m1.map(_.take(2 * 2).splitAt(2)).forall(_.takeRight(2) == _.reverse)

m1.map(_.take(6 * 2).splitAt(6)).map((l,r) => l.takeRight(6) -> r.reverse)

//def findVCross(m: Seq[String]): Option[Int] =
//  (1 until m.head.length).map(i = splitAndCheck(m, i))

(1 until m1.head.length).map(i => splitAndCheck(m1, i))
val m1t = m1.transpose.map(_.mkString)
(1 until m1t.head.length).map(i => splitAndCheck(m1t, i))

(1 until m2.head.length).map(i => splitAndCheck(m2, i))
val m2t = m2.transpose.map(_.mkString)
(1 until m2t.head.length).map(i => splitAndCheck(m2t, i))

findMirrors(m1)
findMirrors(m2)