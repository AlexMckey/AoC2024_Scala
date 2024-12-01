package year2024

import test.*
import parse.given
import day01.*

object Day01_Sample extends PuzzleStringTest(Day01,"3   4\n4   3\n2   5\n1   3\n3   9\n3   3", Some(11), Some(31))

object Day01_Main extends PuzzleFileTest(Day01, Some(1151792), Some(21790168))
