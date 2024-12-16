import scala.annotation.tailrec

class Maz {
  def Maz() = {}
  def abs(x: Double): Double = {
    if (x < 0) -x else x
  }

  def sqrIter(x: Double): Double = {
    sqrIter(x, 1.0)
  }

  def sqrIter(x: Double, estimation: Double): Double = {
    val newEstimation = (estimation + (x / estimation)) / 2.0

    if (abs(estimation - newEstimation) < 0.000000001)
      newEstimation
    else
      sqrIter(x, newEstimation)
  }

  def factorial(n: Int): Int = {
    if (n == 1) 1
    else n * factorial(n - 1)
  }
}

object Maz {
  def main(args: Array[String]): Unit = {
    val maz = new Maz()
    println(maz.sqrIter(9))
    println(maz.sqrIter(2))
    println(maz.factorial(3))
  }
}