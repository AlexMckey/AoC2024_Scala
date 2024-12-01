package year2023.day03

import year2023.DayOf2023
import parse.{*, given}
import coord.{Pos, pos}
import exts.*

import scala.util.matching.Regex

opaque type Parts = Seq[Part]

given Read[Parts] with
  override def read(input: String): Parts =
    val regP: Regex = "([^.\\d]+)|(\\d+)".r
    input.asStrs.zipWithIndex.flatMap { case (ps, y) =>
      regP.findAllMatchIn(ps).map(m => Part(Pos(m.start, y), m.matched)).toList
    }

extension (dt: Parts)
  def partNumbers: Seq[Part] = dt.filter(_.s.head.isDigit)
  def parts: Seq[Part] = dt.filterNot(_.s.head.isDigit)
  def validPartNumbers: Seq[Part] =
    partNumbers
      .filter(pn => parts
        .flatMap(_.p.nearAll)
        .exists(pn.contains))
  def gearPartsNumbers: Seq[Seq[Part]] =
    val gearParts = parts.filter(_.s == "*")
    gearParts
      .map(pg => partNumbers
        .filter(pn => pg.p.nearAll.exists(pn.contains)))
      .filter(_.size == 2)

case class Part(p: Pos, s: String):
  def contains(op: Pos): Boolean = p.y == op.y && op.x >= p.x && op.x <= p.x + s.length - 1

object Day03 extends DayOf2023[Parts](3, "Gear Ratios"):

  override def part1(allParts: Parts): Int =
    allParts.validPartNumbers.map(_.s.toInt).sum

  override def part2(allParts: Parts): Int =
    allParts
      .gearPartsNumbers
      .map(_.map(_.s.toInt).product)
      .sum

//Day 3: Gear Ratios
//  prep: 55.4ms
//  part 1: 209.ms - 560670
//  part 2: 75.0ms - 91622824
