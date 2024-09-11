package year2023

import test.*
import parse.given
import day04.{*, given}

val day04_s1 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
object Day04_Ex1 extends PuzzleStringTest(Day04, day04_s1, Some(13), Some(30))

object Day04_Main extends PuzzleFileTest(Day04, Some(27845), Some(9496801))
