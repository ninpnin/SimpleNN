/**
  * Created by vaino on 12.2.2017.
  */
import Helper.getEntry
import java.io.PrintWriter
import scala.collection.mutable.Buffer
import scala.io.Source

//Class for creating example data
object DataCreator {

  //Matemaattiset funktiot, R^2 => R
  def function1(x: Double, y: Double): Double = x + y * 2
  def function2(x: Double, y: Double): Double = x * x - x * y

  def stringify(input: Vector[Double], output: Vector[Double]) = {
    var str = ""
    for (i <- 0 until input.size - 1) str += input(i) + ","
    str += input.last + "->"
    for (i <- 0 until output.size - 1) str += output(i) + ","
    str += output.last
    str
  }

  val str = stringify(Vector(1,2), Vector(2.4,23.2))
  //println(str)

  def readString(str: String) = {
    val input = Buffer[Double]()
    val output = Buffer[Double]()

    var inputStr = str.take(str.indexOf('>')-1)
    var outputStr = str.drop(str.indexOf('>') + 1)

    for (number <- inputStr.split(',')) {
      input += number.toDouble
    }

    for (number <- outputStr.split(',')) {
      output += number.toDouble
    }

    (input.toVector, output.toVector)
  }

  println(readString(str))


  def writeTo(filename: String, data: Vector[(Vector[Double],Vector[Double])]) = {

    val file = new PrintWriter(filename)
    for (pair <- data) {
      val str = stringify(pair._1,pair._2)
      file.println(str)
    }
    file.close()
  }


  val bufferi = Buffer[(Vector[Double],Vector[Double])]()
  val data = (for (i <- 0 until 1200) yield {
    val x = getEntry / 4
    val y = getEntry / 4

    val f1 = function1(x,y)//x * x - 3 * y * x + y

    val f2 = function2(x,y)// = 1 / x + 1 / y

    val pair: (Vector[Double], Vector[Double]) = (Vector(x, y), Vector(f1, f2))
    pair

  }).toVector


  //writeTo("data.txt", data)

  def getData(filename: String): Vector[(Vector[Double], Vector[Double])] = {
    val rows: Vector[String] = Source.fromFile(filename).getLines().toVector.map(_.trim)
    rows.map(readString(_))
  }
}
