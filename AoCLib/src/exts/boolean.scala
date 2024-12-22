package exts

extension (self: Boolean)
  def flatOption[A](fa: => Option[A]): Option[A] = if self then fa else None
  def either[A, B](b: => B, a: => A): Either[A, B] = Either.cond(self, b, a)