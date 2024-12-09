package year2024.day09

import year2024.DayOf2024
import parse.{*, given}
import coord.{Pos, given}
import grid.MapGrid
import grid.CharGrid
import grid.MapGrid.charMapGridReader

import scala.annotation.tailrec

enum FS(len: Int):
  case File(id: Int, len: Int) extends FS(len)
  case Free(len: Int) extends FS(len)

import FS.*

type I = List[FS]

given Read[List[FS]] with
  override def read(input: String): List[FS] =
    input
      .map(_.asDigit)
      .grouped(2)
      .zipWithIndex
      .flatMap {
        case (Seq(file, free), id) => List(File(id, file), Free(free))
        case (Seq(file), id) => List(File(id, file))}
      .toList

object Day09 extends DayOf2024[I](9, "Disk Fragmenter"):

  def denseCompact(fs: List[FS]): List[FS] =
    @tailrec
    def rec(curfs: List[FS], acc: List[FS] = List.empty): List[FS] =
      curfs match
        case (f @ File(_, _)) +: tail => rec(tail, f +: acc)
        case (init @ Free(_) +: _) :+ Free(_) => rec(init, acc)
        case Free(s) +: middle :+ (f @ File(_, l)) if s == l => rec(middle, f +: acc)
        case Free(s) +: middle :+ (f @ File(_, l)) if s > l => rec(Free(s - l) +: middle, f +: acc)
        case Free(s) +: middle :+ File(id, l) if s < l => rec(middle :+ File(id, l - s), File(id, s) +: acc)
        case Seq(Free(_)) | Seq() => acc

    rec(fs).reverse

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

  def checksum(ls: List[FS]): Long =
    ls.foldLeft(0L -> 0) {
      case ((acc, idx), File(id, len)) => (acc + id * len * (2 * idx + (len - 1)).toLong / 2) -> (idx + len)
      //case ((acc, idx), File(id, len)) => (acc + id * (idx until idx + len).sum.toLong) -> (idx + len)
      case ((acc, idx), Free(len)) => acc -> (idx + len)
    }._1

  override def part1(fs: I): Long =
    denseCompact.andThen(checksum)(fs)

  override def part2(fs: I): Long =
    sparseCompact.andThen(checksum)(fs)

//Day 9: Disk Fragmenter
//  parse : 44.6ms
//  part 1: 1.68s -> 6448989155953
//  part 2: 2.44s -> 6476642796832