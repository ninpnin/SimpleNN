/**
  * Created by vaino on 12.2.2017.
  */
import Helper.getEntry
import Helper.difference


object One extends App {

  val speed = 0.07
  val rounds = 10000

  val inputs = 1
  val hidden = 5
  val outputs = 1

  var WD: Array[Array[Double]] = Array.ofDim[Double](inputs, hidden)
  var WU: Array[Array[Double]] = Array.ofDim[Double](hidden, outputs)

  var deltaWD: Array[Array[Double]] = Array.ofDim[Double](inputs, hidden)
  var deltaWU: Array[Array[Double]] = Array.ofDim[Double](hidden, outputs)

  def nullify() = {
    for (a <- 0 until inputs; b <- 0 until hidden) deltaWD(a)(b) = 0.0
    for (i <- 0 until hidden; j <- 0 until outputs) deltaWU(i)(j) = 0.0
  }
  def shuffle() {
    for (a <- 0 until inputs; b <- 0 until hidden) WD(a)(b) = getEntry
    for (i <- 0 until hidden; j <- 0 until outputs) WU(i)(j) = getEntry
  }

  def activation(e: Double): Double = 1/(1+ Math.exp(-e))
  def derivative(e: Double): Double = Math.exp(-e)/((1+ Math.exp(-e))* (1+ Math.exp(-e)))

  def xD(x: Vector[Double]): Vector[Double] = {
    require(x.length == inputs)
    (for (b <- 0 until hidden) yield {
      val sum: Vector[Double] = (for (a <- 0 until inputs) yield (WD(a)(b) * x(a)) ).toVector
      sum.sum
    }).toVector
  }

  def xU(x: Vector[Double]): Vector[Double] = {
    require(x.length == hidden)
    (for (e <- 0 until hidden) yield activation(x(e))).toVector
  }
  def derivativexU(x: Vector[Double]): Vector[Double] = {
    require(x.length == hidden)
    (for (e <- 0 until hidden) yield derivative(x(e))).toVector
  }

  def xHat(x: Vector[Double]): Vector[Double] = {
    require(x.length == hidden)
    (for (b <- 0 until outputs) yield {
      val sum: Vector[Double] = (for (a <- 0 until hidden) yield (WU(a)(b) * x(a)) ).toVector
      sum.sum
    }).toVector
  }

  shuffle()


  def approx(in: Vector[Double]) = xHat(xU(xD(in)))

  def error(in: Vector[Double], out: Vector[Double]) = {
    val xHat = approx(in)
    Math.sqrt(difference(out, xHat))
  }
  def errorFunction(data: Vector[(Vector[Double], Vector[Double])]) = {
    (for (dataPoint <- data) yield {
      error(dataPoint._1, dataPoint._2)
    }).sum
  }

  val dataSet: Vector[(Vector[Double], Vector[Double])] = (for (n <- 0 until 100) yield {

    val x = getEntry

    (Vector(x),Vector(x* x *x - 3))

  }).toVector
  println(dataSet.take(10))
  println(errorFunction(dataSet))

  def deltaU(i: Int, alpha: Int, beta: Int, dataPoint: Vector[Double]): Double = {
    require(dataPoint.length == inputs)
    if (i == beta) {
      xU(xD(dataPoint))(alpha)
    } else {
      0.0
    }
  }

  def deltaD(i: Int, alpha: Int, beta: Int, dataPoint: Vector[Double]): Double = {
    require(dataPoint.length == inputs)
    WU(beta)(i) * derivativexU(xD(dataPoint))(beta) * dataPoint(alpha)
  }


  for (round <- 0 until rounds) {
    nullify()
    for (pair <- dataSet) {
      for (op <- 0 until outputs) {
        for (a <- 0 until inputs; b <- 0 until hidden) {
          val deriv = deltaU(op, a, b, pair._1)
          val add = deriv * (pair._2(op) - approx(pair._1)(op)) * speed
          deltaWD(a)(b) = deltaWD(a)(b) + add
        }
        for (i <- 0 until hidden; j <- 0 until outputs) {
          val deriv = deltaU(j, i, j, pair._1)
          val add = deriv * (pair._2(op) - approx(pair._1)(op)) * speed * speed
          if (deriv != 0) deltaWU(i)(j) = deltaWU(i)(j) + add
        }
      }
    }

    for (a <- 0 until inputs; b <- 0 until hidden) WD(a)(b) = deltaWD(a)(b) + WD(a)(b)
    for (i <- 0 until hidden; j <- 0 until outputs) WU(i)(j) = deltaWU(i)(j) + WU(i)(j)

    println(errorFunction(dataSet))

  }

  println("WD0,0: " + WD(0)(0))

}
