package year2023.day02

import year2023.DayOf2023
import parse.{*, given}

trait Possible:
  def isValid: Boolean

case class Cube(cnt: Int, color: String) extends Possible:
  def isValid: Boolean = color match
    case "red" => cnt <= 12
    case "green" => cnt <= 13
    case "blue" => cnt <= 14

case class Round(cubes: List[Cube]) extends Possible:
  def isValid: Boolean = cubes.forall(_.isValid)

case class Game(id: Int, rounds: List[Round]) extends Possible:
  def isValid: Boolean = rounds.forall(_.isValid)
  def power: Int = rounds
    .flatMap(r => r.cubes)
    .groupMapReduce(c => c.color)(c => c.cnt)(_ max _)
    .values
    .product

type I = List[Game]

given Read[Cube] = Read("""(\d+) (.+)""".r)
given lc: Read[List[Cube]] = Read(", ")
given Read[Round] = Read("""(.+)""".r)
given Read[List[Round]] = Read("; ")
given Read[Game] = Read("""Game (\d+): (.+)""".r)
given Read[I] = Read("\n")

object Day02 extends DayOf2023[I](2, "Cube Conundrum"):

  override def part1(games: I): Int =
    games
      .filter(_.isValid)
      .map(_.id)
      .sum

  override def part2(games: I): Int =
    games.map(_.power).sum

//Day 2: Cube Conundrum
//  prep: 87.0ms
//  part 1: 2.64ms - 2416
//  part 2: 1.83ms - 63307
