package year2024

import test.*
import parse.given
import day02.*

object Day02_Sample extends PuzzleStringTest(Day02,"7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9", Some(2), Some(4))

object Day02_Main extends PuzzleFileTest(Day02, Some(432), Some(488))
