package year2023

import test.*
import parse.given
import day09.{*, given}

val day09_s1 = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"
object Day09_Ex1 extends PuzzleStringTest(Day09, day09_s1, Some(114), Some(2))
object Day09_Ex2 extends PuzzleStringTest(Day09_, day09_s1, Some(114), Some(2))

object Day09_Main1 extends PuzzleFileTest(Day09, Some(1581679977), Some(889))
object Day09_Main2 extends PuzzleFileTest(Day09_, Some(1581679977), Some(889))