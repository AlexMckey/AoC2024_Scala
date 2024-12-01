package test

import puzzle.Puzzle
import utest.*

import scala.runtime.stdLibPatches.Predef.assert

object SimpleTest extends TestSuite:
  val tests = Tests {
    test("Simple") {
      assert(List(1, 2, 3).length == 3)
      val s = "А роза упала на лапу Азора".replace(" ", "").toLowerCase
      assert(s.reverse == s)
    }
    test("AnotherTest") {
      import Puzzle.dataDir
      assert(dataDir.contains("Input"))
      dataDir
    }
  }