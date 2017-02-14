/**
  * Created by vaino on 12.2.2017.
  */

import Helper.getEntry
import Helper.difference
import Helper.exp2

class Neural(act: Activation, hyper: (Int, Int, Int)) {

  val inputs = hyper._1
  val hidden = hyper._2
  val outputs = hyper._3

  var WD: Array[Array[Double]] = Array.ofDim[Double](inputs, hidden)
  var WU: Array[Array[Double]] = Array.ofDim[Double](hidden, outputs)

  var deltaWD: Array[Array[Double]] = Array.ofDim[Double](inputs, hidden)
  var deltaWU: Array[Array[Double]] = Array.ofDim[Double](hidden, outputs)

  //Reset derivative arrays
  def nullify() = {
    for (a <- 0 until inputs; b <- 0 until hidden) deltaWD(a)(b) = 0.0
    for (i <- 0 until hidden; j <- 0 until outputs) deltaWU(i)(j) = 0.0
  }
  def shuffle() {
    for (a <- 0 until inputs; b <- 0 until hidden) WD(a)(b) = getEntry * 2
    for (i <- 0 until hidden; j <- 0 until outputs) WU(i)(j) = getEntry
  }

  def activation(e: Double): Double = act.function(e)
  def derivative(e: Double): Double = act.derivative(e)

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

  def approx(in: Vector[Double]) = xHat(xU(xD(in)))

  def error(in: Vector[Double], out: Vector[Double]) = {
    val xHat = approx(in)
    difference(out, xHat)
  }

  def errorDerivative(in: Vector[Double], out: Vector[Double]): Vector[Double] = {
    val xHat = approx(in)
    (out zip xHat).map(x => 2 * (x._1 - x._2) )
  }

  def errorFunction(data: Vector[(Vector[Double], Vector[Double])]) = {
    (for (dataPoint <- data) yield {
      error(dataPoint._1, dataPoint._2)
    }).sum
  }

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

  def errorGradientU(data: Vector[(Vector[Double], Vector[Double])]) = {
    nullify()
    val deltaWUclone = deltaWU.clone()
    for (a <- 0 until hidden; b <- 0 until outputs) {
      for (dataPoint <- data) {
        val outer = errorDerivative(dataPoint._1, dataPoint._2)
        val inner = (for (i <- 0 until outputs) yield {
          deltaU(i, a, b, dataPoint._1)
        }).toVector
        val total = (outer zip inner).map(x => -x._1 * x._2).sum
        deltaWUclone(a)(b) = deltaWUclone(a)(b) + total
      }
    }
    deltaWUclone
  }

  def errorGradientD(data: Vector[(Vector[Double], Vector[Double])]) = {
    nullify()
    val deltaWDclone = deltaWD.clone()
    for (a <- 0 until inputs; b <- 0 until hidden) {
      for (dataPoint <- data) {
        val outer = errorDerivative(dataPoint._1, dataPoint._2)
        val inner = (for (i <- 0 until outputs) yield {
          deltaD(i, a, b, dataPoint._1)
        }).toVector
        val total = (outer zip inner).map(x => -x._1 * x._2).sum
        deltaWDclone(a)(b) = deltaWDclone(a)(b) + total
      }
    }
    deltaWDclone
  }

  def optimize(data: Vector[(Vector[Double],Vector[Double])], rounds: Int, speed1: Double, speed2: Double) = {
    require(data(0)._1.length == inputs)
    require(data(0)._2.length == outputs)

    shuffle()

    def prnt = (Helper.random.nextInt(10) == 0)

    for (round <- 0 until rounds) {
      //println(WU(0)(0))
      val gradient1 = errorGradientD(data)
      for (a <- 0 until inputs; b <- 0 until hidden) {
        WD(a)(b) = WD(a)(b) - speed1 * gradient1(a)(b)
      }
      if (prnt)
        println("Gradient 1, Error : ", errorFunction(data), round)
      val gradient2 = errorGradientU(data)
      for (i <- 0 until hidden; j <- 0 until outputs) {
        WU(i)(j) = WU(i)(j) - speed1 * gradient2(i)(j)
      }
      if (prnt)
        println("Gradient 2, Error : ", errorFunction(data))
    }


    while (true) {
      val x = readLine().toString.toDouble
      val y = readLine().toString.toDouble
      println("In", Vector(x,y))
      println("Out", approx(Vector(x,y))(0), approx(Vector(x,y))(1))
      println("("+ DataCreator.function1(x,y) + ", " + DataCreator.function2(x,y) + ")")
    }
  }

}
