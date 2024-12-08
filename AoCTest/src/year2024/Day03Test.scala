package year2024

import test.*
import parse.given
import day03.{ *, given }

object Day03_Sample extends PuzzleStringTest(Day03,"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))", Some(161), Some(48))

object Day03_Main extends PuzzleFileTest(Day03, Some(170778545), Some(82868252))
