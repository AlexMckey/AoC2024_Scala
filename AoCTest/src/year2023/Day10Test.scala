package year2023

import test.*
import parse.given
import day10.{*, given}
import grid.VectorGrid.charVectorGridReader

val day10_s1 = ".....\n.S-7.\n.|.|.\n.L-J.\n....."
object Day10_Ex11 extends PuzzleStringTest(Day10, day10_s1, Some(4), None)
object Day10_Ex12 extends PuzzleStringTest(Day10_, day10_s1, Some(4), None)

val day10_s2 = "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..."
object Day10_Ex21 extends PuzzleStringTest(Day10, day10_s2, Some(8), None)
object Day10_Ex22 extends PuzzleStringTest(Day10_, day10_s2, Some(8), None)

val day10_s3 = "...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n..........."
object Day10_Ex31 extends PuzzleStringTest(Day10, day10_s3, None, Some(4))
object Day10_Ex32 extends PuzzleStringTest(Day10_, day10_s3, None, Some(4))

val day10_s4 = ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ..."
object Day10_Ex41 extends PuzzleStringTest(Day10, day10_s4, None, Some(8))
object Day10_Ex42 extends PuzzleStringTest(Day10_, day10_s4, None, Some(8))

val day10_s5 = ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ..."
object Day10_Ex51 extends PuzzleStringTest(Day10, day10_s5, None, Some(10))
object Day10_Ex52 extends PuzzleStringTest(Day10_, day10_s5, None, Some(10))

object Day10_Main1 extends PuzzleFileTest(Day10, Some(7102), Some(363))
object Day10_Main2 extends PuzzleFileTest(Day10_, Some(7102), Some(363))