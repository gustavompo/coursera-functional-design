package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal ( math.pow(b(),2) - 4 * a() * c() )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      val deltaVal = computeDelta(a, b, c)()
      val bVal = -b()
      val aVal = a()
      val r1 = (bVal + math.sqrt(deltaVal)) / (2 * aVal)
      val r2 = (bVal - math.sqrt(deltaVal)) / (2 * aVal)
      Set(r1, r2)
    }
  }
}
