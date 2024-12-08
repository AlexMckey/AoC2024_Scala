package walker

import coord.{Dir, Pos}

case class Walker(p: Pos, d: Dir):
  def step: Walker = this.copy(p = p.toDir(d))
  def turnRight: Walker = this.copy(d = d.right)
  def turnLeft: Walker = this.copy(d = d.left)
  def goWhile(p: Walker => Boolean): Seq[Walker] =
    Iterator.iterate(this)(w => w.step).takeWhile(p).toSeq
