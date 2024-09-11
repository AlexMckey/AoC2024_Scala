package exts

extension (s: String)
  def splitByBlankLines: Seq[String] = s.split("\n\n").toIndexedSeq.filterNot(_.isEmpty)
  def asStrs: Seq[String] = s.linesIterator.toIndexedSeq.filterNot(_.isEmpty)
  def asInts(sep: String = "\n"): Seq[Int] = s.split(sep).toSeq.filterNot(_.isEmpty).map(_.toInt)
  def asLongs(sep: String = "\n"): Seq[Long] = s.split(sep).toSeq.filterNot(_.isEmpty).map(_.toLong)
  def asWords(sep: String = " "): Seq[String] = s.split(sep).toIndexedSeq.filterNot(_.isEmpty)
  def toIntRadix(radix: Int): Int = Integer.parseInt(s, radix)
  def toLongRadix(radix: Int): Long = java.lang.Long.parseLong(s, radix)
