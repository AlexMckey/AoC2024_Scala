import AoCLib.exts.*
import AoCLib.exts.MapExts.*
import AoCLib.intervals.Intervals

import scala.annotation.tailrec
import scala.util.chaining.*
import scala.collection.mutable
import math.Numeric.Implicits.infixNumericOps

val s =
  "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}"
val Seq(rs, ps) = s.splitByBlankLines

enum Status:
  case Accept, Reject

import Status.*

case class Part(x: Int, m: Int, a: Int, s: Int):
  val rating: Int = x + m + a + s

object Part:
  def apply(s: String): Part = s match
    case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt)

trait Predicate(val rule: String):
  def check: Part => Option[String]

case class Cond(category: String, cond: String, par: Int, override val rule: String) extends Predicate(rule):
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

case class Default(override val rule: String) extends Predicate(rule):
  override def check: Part => Option[String] =
    _ => Some(rule)

val predRegex = "([axms])([<>])(\\d+):(\\S+)".r

object Predicate:
  def apply(s: String): Predicate = s match
    case predRegex(cat, cond, par, rule) => Cond(cat, cond, par.toInt, rule)
    case s"$rule"                        => Default(rule)

def findRule(predicates: Array[Predicate]): Part => String = part =>
  predicates
    .map(_.check)
    .collectFirst { case f if f(part).isDefined => f(part).get }
    .get

val ruleRegex = "(\\S+)\\{(.+)}".r

val parts: Seq[Part] = ps.asStrs.map(Part.apply)
given rules: Map[String, Array[Predicate]] = rs.asStrs.map { case ruleRegex(name, preds) =>
  name -> preds.split(',').map(Predicate.apply)
}.toMap
val ruleFinders: Map[String, Part => String] = rules.map((name, rule) => name -> findRule(rule))
//
//val p1 = parts.head
//val r1 = ruleFinders("in")(p1)
//val r2 = ruleFinders(r1)(p1)
//val r3 = ruleFinders(r2)(p1)
//ruleFinders(r3)(p1)

def workflow(p: Part): Status =
  @tailrec
  def rec(curWF: String): Status =
    ruleFinders(curWF)(p) match
      case "A"  => Accept
      case "R"  => Reject
      case name => rec(name)
  rec("in")

//workflow(p1)
//
//parts.map(workflow)
//parts.filter(workflow(_) == Accept).map(_.rating).sum

extension [A: Numeric](i: Intervals[A])
  def splitAt(a: A): (Intervals[A], Intervals[A]) =
    i.trimAbove(a - summon[Numeric[A]].one) -> i.trimBelow(a)

val cats      = Set("a", "s", "m", "x")
val catChecks = cats.map(cat => cat -> Intervals(1, 4000)).toMap

var rn = "in"
var r: Cond = rules(rn)(0).asInstanceOf[Cond]
var acc = catChecks
val (li1, ri1) = acc(r.category).splitAt(r.par)
var la1 = acc + (r.category -> li1)
acc = acc + (r.category -> ri1)
var r1 = r.rule -> la1
var rd = rules(rn).last.asInstanceOf[Default]
var dr = rd.rule -> acc

rn = r1._1
r = rules(rn)(0).asInstanceOf[Cond]
acc = r1._2
val (li2_1, ri2_1) = acc(r.category).splitAt(r.par)
la1 = acc + (r.category -> li2_1)
acc = acc + (r.category -> ri2_1)
r1 = r.rule -> la1
r = rules(rn)(1).asInstanceOf[Cond]
val (li2_2, ri2_2) = acc(r.category).splitAt(r.par+1)
la1 = acc + (r.category -> ri2_2)
acc = acc + (r.category -> li2_2)
var r2 = r.rule -> la1
rd = rules(rn).last.asInstanceOf[Default]
dr = rd.rule -> acc

def rec(p: Predicate, is: Map[String, Intervals[Int]]): ((String, Map[String, Intervals[Int]]), Map[String, Intervals[Int]]) =
  p match
    case Cond(cat,"<",par,rule) =>
      val (l,r) = is(cat).splitAt(par)
      (rule -> (is + (cat -> l))) -> (is + (cat -> r))
    case Cond(cat,">",par,rule) =>
      val (l,r) = is(cat).splitAt(par+1)
      (rule -> (is + (cat -> r))) -> (is + (cat -> l))
    case Default(rule) => (rule -> is) -> is

var res = rec(rules("in")(0),catChecks)
res._1
res = rec(rules("in")(1),res._2)
res._1

def adjustRule(ruleName: String, im: Map[String, Intervals[Int]])(using rules: Map[String, Array[Predicate]]): Seq[(String, Map[String, Intervals[Int]])] =
  rules(ruleName)
    .foldLeft(im -> Seq.empty[(String, Map[String, Intervals[Int]])]){ case ((acc,res),p) =>
    val r = rec(p,acc)
    r._2 -> (res :+ r._1)
  }._2

val rr1 = adjustRule("in", catChecks)
adjustRule.tupled(rr1.head)

@tailrec
def calcAR(from: Seq[(String, Map[String, Intervals[Int]])],
           accAR: Seq[(String, Map[String, Intervals[Int]])] = Seq.empty): Seq[(String, Map[String, Intervals[Int]])] =
  if from.isEmpty
  then accAR
  else
    val res = adjustRule.tupled(from.head)
    val (ar, other) = res.partition((name,_) => "AR".contains(name))
    calcAR(from.tail ++ other, accAR ++ ar)

val fullI = Intervals(1, 4000)

val resres = calcAR(Seq("in" -> catChecks))
val rra = resres.filter(_._1 == "A")
val rraa1 = rra.head._2
val rraa2 = rra(1)._2
merge(rraa1,rraa2){
  case (i1, Some(i2)) => (fullI - (fullI - i1)) | (fullI - (fullI - i2))
  case (i1, None) => i1
}
rra.map(_._2).reduce((rraa1, rraa2) =>
  merge(rraa1,rraa2){
    case (i1, Some(i2)) => (fullI - (fullI - i1)) | (fullI - (fullI - i2))
    case (i1, None) => i1
  }
)
rra.foreach(println)

val rrr = resres.filter(_._1 == "R")
val rrra1 = rra.head._2
val rrra2 = rra(1)._2
merge(rrra1,rrra2){
  case (i1, Some(i2)) => (fullI - (fullI - i1)) | (fullI - (fullI - i2))
  case (i1, None) => i1
}
rrr.map(_._2).reduce((rraa1, rraa2) =>
  merge(rraa1,rraa2){
    case (i1, Some(i2)) => (fullI - (fullI - i1)) | (fullI - (fullI - i2))
    case (i1, None) => i1
  }
)
rrr.foreach(println)
rra.map(_._2.values.map(_.size.toLong).product).sum

def checkPart(p: Part, is: Map[String, Intervals[Int]]): Boolean =
  is("a").contains(p.a) &&
  is("m").contains(p.m) &&
  is("s").contains(p.s) &&
  is("x").contains(p.x)
  
parts.flatMap(p => rra.collectFirst{ case is if checkPart(p, is._2) => p })
     .map(_.rating).sum