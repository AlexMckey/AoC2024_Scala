package year2023

import test.*
import parse.given
import day07.{*, given}

val day07_s1 = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
object Day07_Ex1 extends PuzzleStringTest(Day07, day07_s1, Some(6440), Some(5905))

object Day07_Main1 extends PuzzleFileTest(Day07, Some(253954294), Some(254837398))
