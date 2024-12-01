package puzzle

import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets
import scala.io.AnsiColor.*

import parse.Read
import common.*

type Result = Int | Long | String

object Puzzle:
  val dataDir: String = """d:/YandexDisk/DevsExercises/AoC/AoCInput"""
  //val dataDir: String = """c:\Users\makievskyav\IdeaProjects\AoC\AoCInput\"""

abstract class Puzzle[I : Read]
  (val year: Int,
   val day: Int,
   val Title: String = ""):
  
  var params: Map[String, String] = Map.empty
  def prep(input: String): I = summon[Read[I]].read(input)
  def part1(data: I): Result = ???
  def part2(data: I): Result = ???

  private val inputFile: Path = Path.of(f"${Puzzle.dataDir}/$year/day$day%02d.txt")

  def main(args: Array[String]): Unit =
    print(s"${CYAN}Day $day: ")
    println(
      if Title.nonEmpty
      then s"$BLUE$BOLD$Title"
      else s"${MAGENTA}Day$day Problem")
    if !Files.exists(inputFile)
    then
      println(s"${RED}File with data for day $day not found")
      println(s"${YELLOW}Filepath: $inputFile")
    else
      print(s"$RESET")
      run(Files.readString(inputFile, StandardCharsets.UTF_8))

  protected def run(input: String): Unit =
    val (data, parseTime) = time(prep(input))
    val (res1, res1Time) = time(part1(data))
    val (res2, res2Time) = time(part2(data))
    show(s"$YELLOW  parse ", parseTime)
    show(s"$MAGENTA  part 1", res1Time, res1)
    show(s"$MAGENTA  part 2", res2Time, res2)

  private def show(prefix: String, time: Long = 0L, result: Result = ""): Unit =
    val (took, unit, color) = time.toDouble / 1000000 match
      case ms if ms > 1000 => ((ms / 1000).toString, "s", Console.RED)
      case ms if ms > 100  => (ms.toString, "ms", Console.YELLOW)
      case ms              => (ms.toString, "ms", Console.GREEN)
    val rs: String = result.toString
    if rs.isEmpty
    then println(s"$prefix: $color${"%-3.4s".format(took)}$unit$RESET")
    else println(s"$prefix: $color${"%-3.4s".format(took)}$unit$MAGENTA -> $RESET$BOLD$rs$RESET")