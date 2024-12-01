package year2023

import parse.Read
import puzzle.Puzzle

class DayOf2023[I : Read](day: Int, title: String = "") extends Puzzle[I](2023, day, title)
