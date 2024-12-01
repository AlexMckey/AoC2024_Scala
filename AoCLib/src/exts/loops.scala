package exts

import scala.annotation.tailrec

extension (n: Int)
  def repeated[A](a: A)(f: A => A): A =
    @tailrec
    def aux(n: Int, acc: A): A =
      if(n > 0) aux(n - 1, f(acc))
      else acc
    aux(n, a)

def repeated[A](f: A => A, n: Int): A => A =
  (0 until n).foldLeft(identity[A])((ff, _) => ff.andThen(f))