package math

import scala.annotation.tailrec
import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.infixOrderingOps

extension [A: Integral](n: A)
  def %+(d: A): A = (n % d + d) % d
  
  def minMax(b: A): (A, A) =
    if n <= b then n -> b else b -> n

def extendedGcd[A](a: A, b: A)(using aIntegral: Integral[A]): ((A, A), A, (A, A)) =

  @tailrec
  def helper(s: A, oldS: A, t: A, oldT: A, r: A, oldR: A): ((A, A), A, (A, A)) =
    if r == 0 then
      ((oldS, oldT), oldR, (s, t))
    else
      val q = oldR / r
      helper(oldS - q * s, s, oldT - q * t, t, oldR - q * r, r)

  helper(aIntegral.zero, aIntegral.one, aIntegral.one, aIntegral.zero, b, a)

def gcd[A: Integral](a: A, b: A): A = extendedGcd(a, b)._2

def gcd[A: Integral](as: Seq[A]): A = as.reduce(gcd(_, _))

def lcm[A: Integral](a: A, b: A): A =
  a / gcd(a, b) * b // divide before multiply to reduce overflow risk

def lcm[A: Integral](as: Seq[A]): A = as.reduce(lcm(_, _))

//@tailrec def gcd[T](a: T, b: T)(using T: Integral[T]): T =
//  import T.*
//  if a < b then gcd(b, a)
//  else if b == zero then a
//  else gcd(b, a % b)
//
//def lcm[T](a: T, b: T)(using T: Integral[T]): T =
//  import T.*
//  if a == zero && b == zero then zero
//  else if a == one then b
//  else if b == one then a
//  else a / gcd(a, b) * b
