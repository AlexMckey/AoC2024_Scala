package year2023.day19_

import puzzle.Puzzle
import common.Default
import parse.{*, given}
import intervals.Segment
import exts.*

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.infixNumericOps
import scala.util.matching.Regex

type CatMapIntervals = CatMap[Segment]
type CatMapPart = CatMap[Int]
type RuleIntervals = (String, CatMapIntervals)
type Rule = (String, Array[Predicate])
type Rules = CatMap[Array[Predicate]]
type DataType = (Seq[Part], Rules)

type CatMap[A] = Map[String, A]

val Categories: Set[String] = Set("a", "m", "s", "x")
val InitCatMapIntervals: CatMapIntervals =
  Categories.map(cat => cat -> Segment.Region(1, 4000)).toMap

case class Part(cats: CatMapPart):
  val rating: Int = cats.values.sum

  def checkPart(is: CatMapIntervals): Boolean =
    cats.forall((cat, par) => is(cat).contains(par))

object Part:
  def apply(s: String): Part = Part(
    s.tail.init.split(',').map { case s"$cat=$value" => cat -> value.toInt }.toMap
  )

trait Predicate(rule: String):
  def toIntervalsAndRest(is: CatMapIntervals): (RuleIntervals, CatMapIntervals)

case class CondAbove(cat: String, par: Int, rule: String) extends Predicate(rule):
  override def toIntervalsAndRest(is: CatMapIntervals): ((String, CatMapIntervals), CatMapIntervals) =
    val (l, r) = is(cat).splitAt(par)
    (rule -> is.updated(cat, l)) -> is.updated(cat, r)

case class CondBelow(cat: String, par: Int, rule: String) extends Predicate(rule):
  override def toIntervalsAndRest(is: CatMapIntervals): ((String, CatMapIntervals), CatMapIntervals) =
    val (l, r) = is(cat).splitAt(par + 1)
    (rule -> is.updated(cat, r)) -> is.updated(cat, l)

case class Default(rule: String) extends Predicate(rule):
  override def toIntervalsAndRest(is: CatMapIntervals): ((String, CatMapIntervals), CatMapIntervals) =
    (rule -> is) -> is

val predRegex: Regex = "([axms])([<>])(\\d+):(\\S+)".r

object Predicate:
  def apply(s: String): Predicate = s match
    case predRegex(cat, ">", par, rule) => CondAbove(cat, par.toInt, rule)
    case predRegex(cat, "<", par, rule) => CondBelow(cat, par.toInt, rule)
    case s"$rule" => Default(rule)

val ruleRegex: Regex = "(\\S+)\\{(.+)}".r

def parseRule(s: String): Rule = s match
  case ruleRegex(name, preds) => name -> preds.split(',').map(Predicate.apply)

given Read[DataType] with
  override def read(input: String): DataType =
    val Seq(rs, ps) = input.splitByBlankLines
    val parts: Seq[Part] = ps.asStrs.map(Part.apply)
    val rules: Rules = rs.asStrs.map(parseRule).toMap
    parts -> rules

object Day19_ extends Puzzle[DataType](2023, 19, "Aplenty"):

  def adjustRule(ruleIntervals: RuleIntervals)(using rules: Rules): Seq[RuleIntervals] =
    rules(ruleIntervals._1)
      .foldLeft(ruleIntervals._2 -> Seq.empty[RuleIntervals])
      { case ((acc, res), p) =>
        val r = p.toIntervalsAndRest(acc)
        r._2 -> (res :+ r._1)
      }._2

  @tailrec
  def calcAR(from: Seq[RuleIntervals], acc: Seq[CatMapIntervals] = Seq.empty)
            (using rules: Map[String, Array[Predicate]]): Seq[CatMapIntervals] =
    if from.isEmpty
    then acc
    else
      val res         = adjustRule(from.head)
      val (ar, other) = res.partition(_._1 == "A")
      calcAR(from.tail ++ other.filterNot(_._1 == "R"), acc ++ ar.map(_._2))

  override def part1(inputData: DataType): Int =
    val (parts, rules)                  = inputData
    given Map[String, Array[Predicate]] = rules
    val acceptedIntervals               = calcAR(Seq("in" -> InitCatMapIntervals))
    parts
      .flatMap(p => acceptedIntervals
        .collectFirst { case is if p.checkPart(is) => p })
      .map(_.rating)
      .sum

  override def part2(inputData: DataType): Long =
    val (_, rules)                      = inputData
    given Map[String, Array[Predicate]] = rules
    calcAR(Seq("in" -> InitCatMapIntervals))
      .map(_.values
            .map(_.size.toLong)
            .product)
      .sum

//Day 18: Lavaduct Lagoon
//  prep: 133.ms
//  part 1: 19.7ms - 68115
//  part 2: 8.43ms - 71262565063800
