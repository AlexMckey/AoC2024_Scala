package math

import scala.math.Integral.Implicits.infixIntegralOps
import scala.annotation.tailrec

object BinarySearch:
  // Find the value n where p(n) is true and p(n - 1) is false
  def binarySearch[A, N: Integral](start: N, end: N)(result: N => A)(p: A => Boolean)
                                  (using CanEqual[N, N]): (N, A) =
    assert(!p(result(start)), "p(start) must be false")
    assert(p(result(end)), "p(end) must be true")

    @tailrec
    def helper(start: N, end: N)(using n: Integral[N]): (N, A) =
      if end == start then
        (end, result(end))
      else
        val two        = n.fromInt(2)
        val half       = start / two + end / two
        val halfResult = result(half)
        if p(halfResult) then
          helper(start, half)
        else
          helper(half + n.one, end)

    helper(start, end)