package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal({
      val d = delta()
      if (d < 0) Set()
      else {
        val v1 = (-b() + math.sqrt(d)) / (2 * a())
        val v2 = (-b() - math.sqrt(d)) / (2 * a())
        Set(v1, v2)
      }
    })
  }
}
