package year2023.day19

import puzzle.Puzzle
import common.Default
import parse.{*, given}
import intervals.Intervals
import exts.*

import scala.annotation.tailrec
import scala.util.matching.Regex

enum Status:
  case Accept, Reject

import Status.*

import Predicate.*

case class Part(x: Int, m: Int, a: Int, s: Int):
  val rating: Int = x + m + a + s

  def checkPart(is: Map[String, Intervals[Int]]): Boolean =
    is("a").contains(a) &&
      is("m").contains(m) &&
      is("s").contains(s) &&
      is("x").contains(x)

enum Predicate(rule: String):
  case Cond(category: String, cond: String, par: Int, rule: String) extends Predicate(rule)
  case DefaultRule(rule: String) extends Predicate(rule)

val predRegex: Regex = "([axms])([<>])(\\d+):(\\S+)".r
val ruleRegex: Regex = "(\\S+)\\{(.+)}".r

extension (s: String)
  def asPredicate: Predicate = s match
    case predRegex(cat, cond, par, rule) => Cond(cat, cond, par.toInt, rule)
    case s"$rule" => DefaultRule(rule)
    
extension (s: String)
  def asPart: Part = s match
    case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt)

type DataType = (Seq[Part], Map[String, Array[Predicate]])

given Read[DataType] with
  override def read(input: String): DataType =
    val Seq(rs, ps) = input.splitByBlankLines
    val parts: Seq[Part] = ps.asStrs.map(asPart)
    val rules: Map[String, Array[Predicate]] = rs.asStrs.map {
      case ruleRegex(name, preds) => name -> preds.split(',').map(asPredicate)
    }.toMap
    parts -> rules

object Day19 extends Puzzle[DataType](2023, 19, "Aplenty"):

//  type DataType = (Seq[Part], Map[String, Array[Predicate]])

  type CatIntervals = Map[String, Intervals[Int]]

  def predToItervals(p: Predicate, is: CatIntervals): ((String, CatIntervals), CatIntervals) =
    p match
      case Cond(cat, "<", par, rule) =>
        val (l, r) = is(cat).splitAt(par)
        (rule -> (is + (cat -> l))) -> (is + (cat -> r))
      case Cond(cat, ">", par, rule) =>
        val (l, r) = is(cat).splitAt(par + 1)
        (rule -> (is + (cat -> r))) -> (is + (cat -> l))
      case DefaultRule(rule) => (rule -> is) -> is

  def adjustRule(ruleName: String, im: CatIntervals)
                (using rules: Map[String, Array[Predicate]]  ): Seq[(String, CatIntervals)] =
    rules(ruleName)
      .foldLeft(im -> Seq.empty[(String, Map[String, Intervals[Int]])]) { case ((acc, res), p) =>
        val r = predToItervals(p, acc)
        r._2 -> (res :+ r._1)
      }
      ._2

  @tailrec
  def calcAR(from: Seq[(String, CatIntervals)], acc: Seq[CatIntervals] = Seq.empty)
            (using rules: Map[String, Array[Predicate]]): Seq[CatIntervals] =
    if from.isEmpty
    then acc
    else
      val res         = adjustRule.tupled(from.head)
      val (ar, other) = res.partition(_._1 == "A")
      calcAR(from.tail ++ other.filterNot(_._1 == "R"), acc ++ ar.map(_._2))

//  override def prep(input: String): DataType =
//    val Seq(rs, ps)      = input.splitByBlankLines
//    val parts: Seq[Part] = ps.asStrs.map(Part.apply)
//    val rules: Map[String, Array[Predicate]] = rs.asStrs.map { case ruleRegex(name, preds) =>
//      name -> preds.split(',').map(Predicate.apply)
//    }.toMap
//    parts -> rules

  override def part1(inputData: DataType): Int =
    val (parts, rules)                  = inputData
    given Map[String, Array[Predicate]] = rules
    val cats                            = Set("a", "s", "m", "x")
    val catChecks                       = cats.map(cat => cat -> Intervals(1, 4000)).toMap
    val rra                             = calcAR(Seq("in" -> catChecks))
    parts
      .flatMap(p => rra.collectFirst { case is if p.checkPart(is) => p })
      .map(_.rating)
      .sum

  override def part2(inputData: DataType): Long =
    val (_, rules)                      = inputData
    given Map[String, Array[Predicate]] = rules
    val cats                            = Set("a", "s", "m", "x")
    val catChecks                       = cats.map(cat => cat -> Intervals(1, 4000)).toMap
    calcAR(Seq("in" -> catChecks)).map(_.values.map(_.size.toLong).product).sum

//Day 18: Lavaduct Lagoon
//  prep: 133.ms
//  part 1: 19.7ms - 68115
//  part 2: 8.43ms - 71262565063800
