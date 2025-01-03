package year2023

import test.*
import parse.given
import day02.{*, given}

val day02_s1 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
object Day02_Ex1 extends PuzzleStringTest(Day02, day02_s1, Some(8), Some(2286))

object Day02_Main extends PuzzleFileTest(Day02, Some(2416), Some(63307))
