import puzzle.Puzzle
import common.Default
import parse.{*, given}
import exts.*
import intervals.*
import Segment.*

import scala.annotation.tailrec
import scala.util.chaining.*
import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex

val input = "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}"

type Cat = Char
type RuleName = String
type Comp = Char

type CatIntervals = Map[Cat, Segment]

val cats: Set[Cat] = "msxa".toSet

enum Predicate:
  case Cond(cat: Cat, check: Comp, num: Int, rule: RuleName)
  case DefaultRule(rule: RuleName)

import Predicate.*

val predRegex: Regex = "([axms])([<>])(\\d+):(\\S+)".r

extension (s: String)
  def asPredicate: Predicate = s match
    case predRegex(cat, cond, par, rule) => Cond(cat.head, cond.head, par.toInt, rule)
    case s"$rule" => DefaultRule(rule)

type Part = Map[Cat, Int]

extension (s: String)
  def asPart: Part =
    s.tail.init
      .split(',')
      .map { case s"$cat=$value" => cat.head -> value.toInt }
      .toMap

type Predicates = Array[Predicate]

type Rule = (RuleName, Predicates)
type Rules = Map[RuleName, Predicates]
type RuleIntervals = (RuleName, CatIntervals)

extension (s: String)
  def asRule: Rule = s match
    case s"$name{$ps}" => name -> ps.split(",").map(_.asPredicate)

//extension (is: CatIntervals)
//  def adjustPredicate(p: Predicate): (RuleIntervals, CatIntervals) =
//    val cat = p match
//      case Cond(cat, '>', num, rule) if is(cat).contains(num) =>
//      case Cond(cat, '<', num, rule) => (rule -> is) -> is
//      case DefaultRule(rule) => (rule -> is) -> is

type Input = (Rules, Seq[Part])

given Read[Input] =
  _.splitByBlankLines
    .map(_.asStrs)
    .pipe{ case Seq(rs, ps) =>
      rs.map(_.asRule).toMap -> ps.map(_.asPart)
    }

val is = cats.map(_ -> Region(1,4000)).toMap

val (rules, parts) = summon[Read[Input]].read(input)
val pa1 = parts.head
val r1 = rules.head
val pr1 = r1._2.head.asInstanceOf[Cond]
val ct1 = pr1.cat
val (l, r) = is(ct1).splitAt(pr1.num)
(pr1.rule -> is.updated(ct1, l)) -> is.updated(ct1, r)

