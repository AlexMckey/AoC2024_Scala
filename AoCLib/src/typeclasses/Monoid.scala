package typeclasses

trait Semigroup[A]:

  import scala.annotation.targetName

  extension (x: A)
    def combine(y: A): A
    @targetName("add")
    def |+|(y: A): A = x.combine(y)

trait Monoid[A] extends Semigroup[A]:
  def empty: A

given Monoid[String] with
  extension (x: String)
    def combine (y: String): String = x.concat(y)
  def empty: String = ""

given addIntMonoid: Monoid[Int] with
  extension (x: Int)
    def combine (y: Int): Int = x + y
  def empty: Int = 0

given multIntMonoid: Monoid[Int] with
  extension (x: Int)
    def combine(y: Int): Int = x * y
  def empty: Int = 1

given monoidTuple[A: Monoid, B: Monoid]: Monoid[(A, B)] =
    new Monoid[(A, B)]:
      extension (x: (A, B))
        def combine(y: (A, B)): (A, B) =
          val (xa, xb) = x
          val (ya, yb) = y
          (xa.combine(ya), xb.combine(yb))

      def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)


object Monoid:
  def apply[T](using m: Monoid[T]) = m

  def combineAll[T: Monoid](xs: List[T]): T =
    xs.foldLeft(Monoid[T].empty)(_.combine(_))