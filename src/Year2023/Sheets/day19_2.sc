import AoCLib.exts.*
import AoCLib.exts.MapExts.*
//import AoCLib.intervals.Intervals

import scala.annotation.tailrec
import scala.util.chaining.*
import scala.collection.mutable
import math.Numeric.Implicits.infixNumericOps

val s =
  "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}"
val Seq(rs, ps) = s.splitByBlankLines

trait Intervals:
  def contains(p: Int): Boolean
  def splitAt(p: Int): (Intervals, Intervals)
  def size: Int

object Intervals:
  case class Region(low: Int, high: Int) extends Intervals:
    def contains(p: Int): Boolean = p >= low && p <= high
    def splitAt(p: Int): (Intervals, Intervals) =
      if contains(p)
      then Region(low, p - 1) -> Region(p, high)
      else if p > high
           then this  -> Empty
           else Empty -> this
    def size: Int = high - low + 1

    override def toString: String = s"[$low .. $high]"

  object Empty extends Intervals:
    def contains(p: Int): Boolean               = false
    def splitAt(p: Int): (Intervals, Intervals) = Empty -> Empty
    def size: Int = 0
    override def toString: String = s"[..]"

val Categories = Set("a", "s", "m", "x")

case class Part(cats: Map[String, Int]):
  val rating: Int = cats.values.sum

object Part:
  def apply(s: String): Part = Part(s.tail.init.split(',').map {
    case s"$cat=$value" => cat -> value.toInt
  }.toMap)

sealed trait RuleName:
  def rule: String
  def applyPred(is: Map[String, Intervals]): ((String, Map[String, Intervals]), Map[String, Intervals])

enum Predicate extends RuleName:
  case CondAbove(category: String, par: Int, rule: String)
  case CondBelow(category: String, par: Int, rule: String) extends Predicate
  case Default(rule: String) extends Predicate
      
  def applyPred(is: Map[String, Intervals]): ((String, Map[String, Intervals]), Map[String, Intervals]) =
    this match
      case CondAbove(cat, par, rule) =>
        val (l, r) = is(cat).splitAt(par + 1)
        (rule -> (is + (cat -> r))) -> (is + (cat -> l))
      case CondBelow(cat, par, rule) =>
        val (l, r) = is(cat).splitAt(par)
        (rule -> (is + (cat -> l))) -> (is + (cat -> r))
      case Default(rule) => (rule -> is) -> is  

import Predicate.*

val predRegex = "([axms])([<>])(\\d+):(\\S+)".r

object Predicate:
  def apply(s: String): Predicate = s match
    case predRegex(cat, ">", par, rule) => CondAbove(cat, par.toInt, rule)
    case predRegex(cat, "<", par, rule) => CondBelow(cat, par.toInt, rule)
    case s"$rule"                       => Default(rule)

val ruleRegex = "(\\S+)\\{(.+)}".r

val parts: Seq[Part] = ps.asStrs.map(Part.apply)
given rules: Map[String, Array[Predicate]] = rs.asStrs.map { case ruleRegex(name, preds) =>
  name -> preds.split(',').map(Predicate.apply)
}.toMap

val catChecks: Map[String, Intervals] = Categories.map(cat => cat -> Intervals.Region(1, 4000)).toMap

var res = rules("in")(0).applyPred(catChecks)
res._1
res = rules("in")(1).applyPred(res._2)
res._1

def adjustRule(ruleName: String, im: Map[String, Intervals])(using rules: Map[String, Array[Predicate]]): Seq[(String, Map[String, Intervals])] =
  rules(ruleName)
    .foldLeft(im -> Seq.empty[(String, Map[String, Intervals])]){ case ((acc,res),p) =>
    val r = p.applyPred(acc)
    r._2 -> (res :+ r._1)
  }._2

val rr1 = adjustRule("in", catChecks)
adjustRule.tupled(rr1.head)

@tailrec
def calcAR(from: Seq[(String, Map[String, Intervals])],
           accAR: Seq[(String, Map[String, Intervals])] = Seq.empty): Seq[(String, Map[String, Intervals])] =
  if from.isEmpty
  then accAR
  else
    val res = adjustRule.tupled(from.head)
    val (ar, other) = res.partition((name,_) => "AR".contains(name))
    calcAR(from.tail ++ other, accAR ++ ar)

val resres = calcAR(Seq("in" -> catChecks))
val rra = resres.filter(_._1 == "A")
rra.foreach(println)

val rrr = resres.filter(_._1 == "R")
rrr.foreach(println)
rra.map(_._2.values.map(_.size.toLong).product).sum

4000*4000*4000*4000 - rrr.map(_._2.values.map(_.size.toLong).product).sum

def checkPart(p: Part, is: Map[String, Intervals]): Boolean =
  p.cats.forall((cat,v) => is(cat).contains(v))
  
parts.flatMap(p => rra.collectFirst{ case is if checkPart(p, is._2) => p })
     .map(_.rating).sum