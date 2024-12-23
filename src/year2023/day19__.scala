package year2023.day19__

import year2023.DayOf2023
import common.Default
import parse.{*, given}
import exts.*

import scala.util.matching.Regex

type DataType = String

val input = "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}"

object Day19__ extends DayOf2023[DataType](19, "Aplenty"):

  override def part1(inputData: DataType): Int =
    println(inputData)
    1
  override def part2(inputData: DataType): Long = ???

//Day 18: Lavaduct Lagoon
//  prep: 133.ms
//  part 1: 19.7ms - 68115
//  part 2: 8.43ms - 71262565063800
