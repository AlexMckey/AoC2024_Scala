package year2023.day05

import puzzle.Puzzle
import parse.{*, given}
import exts.*

import scala.annotation.tailrec

case class Segment(from: Long, to: Long):
  private def points: Seq[Long] = Seq(from, to + 1)

  def break(segs: Seq[Segment]): Seq[Segment] =
    (segs
      .flatMap(_.points)
      .filter(p => p >= from && p <= to + 1) ++ points).distinct.sorted
      .sliding(2)
      .map(p => Segment(p.head, p.last - 1))
      .toSeq

  override def toString: String = s"[$from,$to]"

object Segment:
  def withLen(from: Long, len: Long): Segment =
    Segment(from, from + len - 1)

case class MapRecord(destFrom: Long, sourceFrom: Long, len: Long):
  def decodeFunc: PartialFunction[Long, Long] =
    case x if x >= sourceFrom && x <= sourceTo => x - sourceFrom + destFrom

  def sourceTo: Long = sourceFrom + len - 1

  def segmentSource: Segment = Segment(sourceFrom, sourceTo)
  
case class Part(what: String, segs: Seq[Segment])

case class DecodeMap(from: String, to: String, maps: Seq[MapRecord]):
  private def decodeFunc(s: Segment) = maps
    .map(_.decodeFunc)
    .collectFirst { case f if f.isDefinedAt(s.from) => Segment(f(s.from), f(s.to)) }
    .getOrElse(s)

  private val mapSegments = maps.map(_.segmentSource)

  def decodePart(p: Part): Part =
    val newSegs = p.segs
      .flatMap(_.break(mapSegments))
      .map(decodeFunc)
    Part(to, newSegs)

type I = (Seq[Long], Seq[DecodeMap])

given Read[I] with
  override def read(input: String): I =
    val blocks = input.splitByBlankLines
    val seeds = blocks.head.split(':').tail.head.asLongs(" ")
    val dms = blocks.tail.map(ss =>
      val sm = ss.asStrs
      val ds = sm.tail.map(sa =>
        val Seq(fr, to, ln) = sa.asLongs(" ")
        MapRecord(fr, to, ln)
      )
      sm.head match
        case s"$from-to-$to map:" => DecodeMap(from, to, ds)
    )
    (seeds, dms)

object Day05 extends Puzzle[I](2023, 5, "If You Give A Seed A Fertilizer"):
  
  @tailrec
  def decodeAll(curPart: Part, maps: Seq[DecodeMap]): Part =
    val dm = maps.find(_.from == curPart.what)
    if dm.isEmpty then curPart
    else decodeAll(dm.get.decodePart(curPart), maps)

  def part(seeds: Seq[Part], decodeMaps: Seq[DecodeMap]): Long =
    val ds = seeds.map(decodeAll(_, decodeMaps))
    ds.map(_.segs.minBy(_.from).from).min

  override def part1(inputData: I): Long =
    val (seeds, maps) = inputData
    part(seeds.map(s => Part("seed", Seq(Segment(s, s)))), maps)

  override def part2(inputData: I): Long =
    val (seeds, maps) = inputData
    part(
      seeds
        .grouped(2)
        .toSeq
        .map(s => Part("seed", Seq(Segment.withLen(s.head, s.last)))),
      maps
    )

//Day 5: If You Give A Seed A Fertilizer
//  prep: 36.7ms
//  part 1: 30.9ms - 535088217
//  part 2: 15.2ms - 51399228

