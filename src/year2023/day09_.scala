package year2023.day09

import year2023.DayOf2023
import parse.{*, given}
import exts.*

type History = Seq[Int]
type I = Seq[History]

given Read[I] = _.asStrs.map(_.asInts(" "))

object Day09_ extends DayOf2023[I](9, "Mirage Maintenance"):

  def differences(history: History): History =
    (history lazyZip history.tail).map(_ - _)

  def prediction(seq: History): Int =
    LazyList
      .iterate(seq)(differences)
      .takeWhile(_.exists(_ != 0))
      .map(_.head)
      .sum

  def part(seqs: I): Int =
    seqs.map(prediction).sum

  override def part1(seqs: I): Int =
    part(seqs.map(_.reverse))

  override def part2(seqs: I): Int =
    part(seqs)

//Day 9: Mirage Maintenance
//  prep: 32.3ms
//  part 1: 34.5ms - 1581679977
//  part 2: 7.78ms - 889
