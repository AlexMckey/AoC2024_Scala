package year2023

import test.*
import parse.given
import day03.{*, given}

val day03_s1 = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
object Day03_Ex1 extends PuzzleStringTest(Day03, day03_s1, Some(4361), Some(467835))

object Day03_Main extends PuzzleFileTest(Day03, Some(560670), Some(91622824))
