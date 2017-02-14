import scala.util.Random

/**
  * Created by vaino on 12.2.2017.
  */
object Helper {

  val random = new Random(2222)
  val upper = 2.4
  val lower = -2.4
  def getEntry: Double = {
    random.nextDouble() * (upper - lower) + lower
  }

  def difference(a: Vector[Double], b: Vector[Double]) = {
    val sumOfSquares = (a zip b).map(x => (x._1 - x._2) * (x._1 - x._2))
    sumOfSquares.sum
  }

  val limit = 4.0
  def fil(d: Array[Array[Double]]) = {
    if (Math.abs(d.flatten.max) > limit) {
      val a = limit * Math.abs(d.flatten.max)

      for (i <- 0 until d.length; j <- 0 until d(0).length) {
        d(i)(j) = d(i)(j) / a
      }
    }
  }

  def exp2(x: Double) = {
    Math.exp(-x * x)
  }

  def getInput(n: Int) = {
    (for (i <- 0 until n) yield getEntry).toVector
  }
}
