package year2023

import test.*
import parse.given
import day01.*

object Day01_S1 extends PuzzleStringTest(Day01,"1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet", Some(142), None)

object Day01_S2 extends PuzzleStringTest(Day01, "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen", None, Some(281))

object Day01_Main extends PuzzleFileTest(Day01, Some(54953), Some(53868))
