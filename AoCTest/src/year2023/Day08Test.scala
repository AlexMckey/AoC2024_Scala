package year2023

import test.*
import parse.given
import day08.{*, given}

val day08_s1 = "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"
object Day08_Ex1 extends PuzzleStringTest(Day08, day08_s1, Some(2), None)

val day08_s2 = "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"
object Day08_Ex2 extends PuzzleStringTest(Day08, day08_s2, Some(6), None)

val day08_s3 = "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)"
object Day08_Ex3 extends PuzzleStringTest(Day08, day08_s3, None, Some(6))

object Day08_Main1 extends PuzzleFileTest(Day08, Some(12169), Some(12030780859469L))
