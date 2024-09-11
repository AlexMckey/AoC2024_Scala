package year2023

import test.*
import parse.given
import day06.{*, given}

val day06_s1 = "Time:      7  15   30\nDistance:  9  40  200"
object Day06_Ex1 extends PuzzleStringTest(Day06, day06_s1, Some(288), Some(71503))

object Day06_Main extends PuzzleFileTest(Day06, Some(138915), Some(27340847))
