package test

import parse.{Read, Show}
import puzzle.{Puzzle, Result}
import utest.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class PuzzleFileTest[I : Read](
  puzzle: Puzzle[I],
  expected1: Option[Result] = None,
  expected2: Option[Result] = None,
  suffix: String = "",
  params: Map[String, String] = Map.empty
                              ) extends TestSuite:
  
  private val year = puzzle.year
  private val day = puzzle.day
  private val suff = if suffix.isEmpty then "" else s".$suffix"
  private val inputFile: Path = Path.of(f"${Puzzle.dataDir}/$year/day$day%02d$suff.txt")

  if !Files.exists(inputFile)
  then println(s"Not found puzzle file: $inputFile")

  puzzle.params = params

  private val data = puzzle.prep(Files.readString(inputFile, StandardCharsets.UTF_8))
  
  val tests = Tests {
    "Day Problem" - {
      test("part1") - {
        if expected1.isDefined
        then
          val res1 = puzzle.part1(data)
          res1 ==> expected1.get
          res1
        else ""  
      }
      test("part2") - {
        if expected2.isDefined
        then
          val res2 = puzzle.part2(data)
          res2 ==> expected2.get
          res2
        else ""  
      }
    }
  }