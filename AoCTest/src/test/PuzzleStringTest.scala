package test

import parse.{Read, Show}
import puzzle.{Puzzle, Result}
import utest.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class PuzzleStringTest[I : Read](
  puzzle: Puzzle[I],
  input: String,
  expected1: Option[Result] = None,
  expected2: Option[Result] = None,
  params: Map[String, String] = Map.empty
                                ) extends TestSuite:
  
  puzzle.params = params

  private val data = puzzle.prep(input)

  val tests: Tests = Tests {
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