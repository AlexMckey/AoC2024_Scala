package year2023.day08

import year2023.DayOf2023
import parse.{*, given}
import exts.*
import exts.iterables.*
import math.lcm

import scala.annotation.tailrec
import scala.util.chaining.*

type DataType = (String, Map[String, MapDir])

type MapDir = (String, String)

given Read[DataType] =
  _.splitByBlankLines.pipe { as =>
    as.head ->
      as.last.asStrs.map {
        case s"$from = ($left, $right)" => from -> (left, right)
      }.toMap
  }

object Day08 extends DayOf2023[DataType](8, "Haunted Wasteland"):

  private def goDir(md: MapDir, dir: Char): String = if dir == 'L' then md._1 else md._2

  @tailrec
  def go(dirs: Iterator[Char], maps: Map[String, MapDir], cur: String, end: String => Boolean, steps: Int = 0): Int =
    if end(cur)
    then steps
    else go(dirs, maps, goDir(maps(cur), dirs.next), end, steps + 1)

  override def part1(inputData: DataType): Int =
    val (ds, ms) = inputData
    val is = ds.cycle
    go(is, ms, "AAA", _ == "ZZZ")

  override def part2(inputData: DataType): Long =
    val (ds, ms) = inputData
    val starts = ms.keys.filter(_.endsWith("A")).toSeq
    lcm(starts.map(s => go(ds.cycle, ms, s, _.endsWith("Z")).toLong))

//Day 8: Haunted Wasteland
//  prep: 100.ms
//  part 1: 16.5ms - 12169
//  part 2: 48.7ms - 12030780859469
