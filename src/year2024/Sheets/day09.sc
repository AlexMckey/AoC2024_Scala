enum FS(len: Int):
  case File(id: Int, len: Int) extends FS(len)
  case Free(len: Int) extends FS(len)
  def checksum(from: Int): Long =
    this match
      case File(id, len) => id * len * (2 * from + (len - 1)) / 2
      case Free(_) => 0

import FS.*
import parse.Read

import scala.annotation.tailrec

val s = "2333133121414131402"
val fs = s.map(_.asDigit).grouped(2).zipWithIndex
  .flatMap{ case (Seq(file,free), id) => List(File(id, file), Free(free))
            case (Seq(file), id) => List(File(id, file))}
  .toList

def compact(fs: List[FS]): List[FS] =
  @tailrec
  def rec(curfs: List[FS], acc: List[FS] = List.empty): List[FS] =
    println(curfs)
    println(acc)
    curfs match
      case Nil => acc
      case File(id,l) +: tail => rec(tail, File(id,l) +: acc)
      case init :+ Free(_) => rec(init, acc)
      case Free(s) +: middle :+ File(id, l) if s > l => rec(Free(s - l) +: middle, File(id,l) +: acc)
      case Free(s) +: middle :+ File(id, l) if s == l => rec(middle, File(id, l) +: acc)
      case Free(s) +: middle :+ File(id, l) if s < l => rec(middle :+ File(id, l-s), File(id, s) +: acc)
  rec(fs).reverse

val r1 = compact(fs)
r1(1).checksum(2)
r1.foldLeft(0L -> 0){ case ((acc, i), f @ File(id, len)) => (acc + f.checksum(i)) -> (i + len)}

def checksumFS(ls: List[FS]): Long =
  ls.foldLeft(0L -> 0) {
    case ((acc, idx), File(id, len)) => (acc + id * len * (2 * idx + (len - 1)).toLong / 2) -> (idx + len)
    //case ((acc, idx), File(id, len)) => (acc + id * (idx until idx + len).sum.toLong) -> (idx + len)
    case ((acc, idx), Free(len)) => acc -> (idx + len)
  }._1

checksumFS(r1)

val d = s.map(_.asDigit).foldLeft((0, false, List.empty[List[Int]])){ case ((id, free, acc), cnt) =>
  if free then (id, !free, List.fill(cnt){-1} +: acc)
  else (id + 1, !free, List.fill(cnt){id} +: acc)
}._3.flatten.reverse.toArray

val disk = fs.flatMap{
  case File(id, l) => List.fill(l){id}
  case Free(0) => List.empty[Int]
  case Free(l) => List.fill(l){-1}
}.toArray

@tailrec
def move(left: Int = 0, right: Int = disk.length-1, ar: Array[Int] = disk): Array[Int] =
  if left == right then
    ar.take(left+1)
  else if left > right then
    ar.take(left)
  else if ar(right) < 0 then
    move(left, right-1, ar)
  else if ar(left) < 0 then
    ar(left) = ar(right)
    move(left + 1, right - 1, ar)
  else
    move(left + 1, right, ar)

val res = move()

def checksum(ar: Array[Int]): Long =
  ar.zipWithIndex.map((id,i) => id * i).sum

checksum(res)

val f1 = fs.last.asInstanceOf[File]
fs.init
val (before, after) = fs.init.span{
  case Free(s) => s < f1.len
  case _ => true
}
val fr = after.head.asInstanceOf[Free]
val repl = if fr.len == f1.len then List(f1) else List(f1, Free(fr.len - f1.len))
val r2 = before ++ repl ++ after.tail
r2.init

val acc = List(Free(5))
val f2 = r2.init.last.asInstanceOf[File]
r2.init.init.span{
  case Free(s) => s < f2.len
  case _ => true
}
  match
    case (_, Nil) => (r2.init, f2 +: acc)
    case (before, Free(s) +: after) if s == f2.len => ((before :+ f2) ++ after, Free(f2.len) :: acc)
    case (before, Free(s) +: after) => ((before :+ f2 :+ Free(s - f2.len)) ++ after, Free(f2.len) :: acc)

def sparseCompact(fs: List[FS]): List[FS] =
  @tailrec
  def rec(curfs: List[FS], acc: List[FS] = List.empty): List[FS] =
    curfs match
      case init :+ (f @ Free(_)) => rec(init, f +: acc)
      case init :+ (f @ File(_, l)) =>
        init.span{
          case Free(s) => s < l
          case _ => true
        } match
          case (_, Nil) => rec(init, f +: acc)
          case (before, Free(s) +: after) if s == l => rec((before :+ f) ++ after, Free(l) :: acc)
          case (before, Free(s) +: after) => rec((before :+ f :+ Free(s - l)) ++ after, Free(l) :: acc)
      case Nil => acc

  rec(fs)

val res2 = sparseCompact(fs)
checksumFS(res2)


import parse.{*, given}
given Read[List[Int]] = Read.seq("")

summon[Read[List[Int]]].read("1251515151")