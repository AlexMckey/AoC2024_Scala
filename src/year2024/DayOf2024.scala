package year2024

import parse.Read
import puzzle.Puzzle

class DayOf2024[I : Read](day: Int, title: String = "") extends Puzzle[I](2024, day, title)
