package jecrevin.math

trait RealNum[T] extends Numerical[T] with Ordering[T]:
  def abs(x: T): T
  override def toComplex(x: T): Complex[T]

  class RealNumOps(lhs: T):
    def abs = RealNum.this.abs(lhs)

  implicit def mkRealNumOps(lhs: T): RealNumOps = new RealNumOps(lhs)
