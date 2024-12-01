import AoCLib.exts.*

import scala.annotation.tailrec
import scala.util.chaining.*
import scala.collection.mutable

val s = "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}"
val Seq(rs, ps) = s.splitByBlankLines

enum Status:
  case Accept, Reject

import Status.*

case class Part(x: Int, m: Int, a: Int, s: Int):
  val rating: Int = x + m + a + s

object Part:
  def apply(s: String): Part = s match
    case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt)

trait Predicate(rule: String):
  def check: Part =>  Option[String]

case class Cond(category: String, cond: String, par: Int, rule: String) extends Predicate(rule):
  val checkF: Part => Boolean =
    val iff: Int => Boolean = if cond == "<" then _ < par else _ > par
    val cat: Part => Int = category match
      case "a" => _.a
      case "x" => _.x
      case "m" => _.m
      case "s" => _.s
    cat andThen iff

  override def check: Part => Option[String] =
    part => Option.when(checkF(part))(rule)

case class Default(rule: String) extends Predicate(rule):
  override def check: Part => Option[String] =
    _ => Some(rule)

val predRegex = "([axms])([<>])(\\d+):(\\S+)".r

object Predicate:
  def apply(s: String): Predicate = s match
    case predRegex(cat,cond,par,rule) => Cond(cat,cond,par.toInt,rule)
    case s"$rule" => Default(rule)

def findRule(predicates: Array[Predicate]): Part => String = part =>
  predicates
    .map(_.check)
    .collectFirst{case f if f(part).isDefined => f(part).get}
    .get

val ruleRegex = "(\\S+)\\{(.+)}".r

val parts: Seq[Part] = ps.asStrs.map(Part.apply)
val rules: Map[String, Part => String] = rs.asStrs.map {
  case ruleRegex(name, preds) => name -> findRule(preds.split(',').map(Predicate.apply))
}.toMap

val p1 = parts.head
val r1 = rules("in")(p1)
val r2 = rules(r1)(p1)
val r3 = rules(r2)(p1)
rules(r3)(p1)

def workflow(p: Part): Status =
  @tailrec
  def rec(curWF: String): Status =
    rules(curWF)(p) match
      case "A" => Accept
      case "R" => Reject
      case name => rec(name)
  rec("in")

workflow(p1)

parts.map(workflow)
parts.filter(workflow(_) == Accept).map(_.rating).sum

