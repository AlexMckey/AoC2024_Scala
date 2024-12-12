package year2024.day04

import year2024.DayOf2024
import parse.{*, given}
import grid.*
import MapGrid.charMapGridReader
import coord.{Pos, Dir, given}
import Dir.{allDirs, diagDirs}

type I = CharGrid

object Day04 extends DayOf2024[I](4, "Ceres Search"):

  override def part1(g: I): Int =
    //MapGrid(s).findAll(4, "XMAS".r, allDirs).size
    g.filter(_ == 'X').allPos.toList
      .map(p => allDirs
        .count(d => List.fill(3){d}
          .scanLeft(p) {_ + _.delta}
          .flatMap(g.get)
          .mkString == "XMAS"))
      .sum

  override def part2(g: I): Int =
    g.filter(_ == 'A').allPos.toList
      .map(p => diagDirs
        .map(d => p + d.delta)
        .flatMap(g.get)
        .mkString)
      .count(List("SMMS", "MMSS", "MSSM", "SSMM").contains)


//Day 4: Ceres Search
//  parse : 93.2ms
//  part 1: 136.ms -> 2549
//  part 2: 30.6ms -> 2003