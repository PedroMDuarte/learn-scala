package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var {
      val bval = b()
      val aval = a()
      val cval = c()
      (bval * bval) - 4.0 * aval * cval
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var {
      val d = delta()
      val aval = a()
      val bval = b()

      if (d < 0) Set()
      if (d == 0) Set(-0.5 * bval / aval)
      else {
        val rt = Math.sqrt(d)
        Set(-0.5 * (bval / aval + rt / aval), -0.5 * (bval / aval - rt / aval))
      }

    }
  }
}
