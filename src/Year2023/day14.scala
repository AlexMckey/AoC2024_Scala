package year2023.day14

import box.Box
import puzzle.Puzzle
import common.Default
import common.Default.default
import parse.{*, given}
import coord.{Coord, Dir, Pos, given}
import grid.{CharGrid, Grid, MapGrid}
import cycle.*
import scala.util.chaining.*

import scala.annotation.tailrec

case class G(box: Box, walls: Set[Pos], stones: Set[Pos])

object G:
  def apply(g: CharGrid): G =
    val b = g.gridBox
    val ws = g.filter(_ == '#').allPos
    val ss = g.filter(_ == 'O').allPos
    G(b, ws, ss)

given Default[Char] = '.'
  
given Read[G] = MapGrid.apply(_).pipe(G.apply)

object Day14 extends Puzzle[G](2023, 14, "Parabolic Reflector Dish"):

//  override def prep(input: String): DataType =
//    val ss = input.asStrs.filterNot(_.isEmpty)
//    val box = Box(Pos.zero, Pos(ss.head.length-1, ss.length-1))
//    val maps = ss.zipWithIndex
//                 .flatMap((row,y) => row.zipWithIndex
//                    .collect{ case (c,x) if c != '.' => Pos(x,y) -> c })
//    box -> maps.toSet
//      .partitionMap{
//        case (p, 'O') => Left(p)
//        case (p, '#') => Right(p)
//      }

  def calcNorthLoad(stones: Set[Pos])(using gridBox: Box): Int =
    stones.foldLeft(0)((acc,p) => acc + gridBox.max.y + 1 - p.y)

  @tailrec
  def fall(toFall: Set[Pos], wall: Set[Pos], d: Dir, stones: Set[Pos] = Set.empty)(using gridBox: Box): Set[Pos] =
    if toFall.isEmpty then stones
    else
      val falling      = toFall.map(_.toDir(d))
      val notFalling   = falling intersect (stones ++ wall)
                      ++ falling.filterNot(gridBox.contains)
      val stopped      = notFalling.map(_.toDir(d.flip))
      val stoneStopped = toFall intersect falling
      val newToFall    = falling -- stopped -- notFalling -- stoneStopped
      fall(stoneStopped.map(_.toDir(d.flip)) ++ newToFall, wall, d, stones ++ stopped)

  def cycleTilt(stones: Set[Pos])(using walls: Set[Pos], gridBox: Box): Set[Pos] =
    Set(Dir.N, Dir.W, Dir.S, Dir.E).foldLeft(stones)((st, d) => fall(st, walls, d))

  override def part1(reflector: G): Int =
    given Box = reflector.box
    val res = fall(reflector.stones, reflector.walls, Dir.N)
    calcNorthLoad(res)

  override def part2(reflector: G): Int =
    given Box = reflector.box
    given Set[Pos] = reflector.walls
    val is = Iterator.iterate(reflector.stones)(cycleTilt)
    val Some((init,cycle)) = detectCycle(is,0): @unchecked
    val cnt = cycledEquivalentIterations(init, cycle, 1000000000).toInt - init
    calcNorthLoad(is.drop(cnt - 1).next)
    
//Day 14: Parabolic Reflector Dish
//  prep: 57.4ms
//  part 1: 43.0ms - 108813
//  part 2: 2.72s - 104533