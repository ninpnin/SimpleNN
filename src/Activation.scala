/**
  * Created by vaino on 13.2.2017.
  */
import scala.math.exp
import scala.math.log

//Class template for different activation functions
abstract class Activation {

  def function(x: Double): Double
  def derivative(x: Double): Double

}

//Sigmoid as the most basic function
object Sigmoid extends Activation {

  def function(x: Double) = 1.0 / (1 + exp(-x))
  def derivative(x: Double) = exp(x) / ((1 + exp(x)) * (1 + exp(x)))

}

object Softplus extends Activation {

  def function(x: Double) = log(1 + exp(x))
  def derivative(x: Double) = 1.0 / (1 + exp(-x))

}

object Tanh extends Activation {

  def function(x: Double) = (exp(x) - exp(-x) )/(exp(x) + exp(-x))
  def derivative(x: Double) = 1 - this.function(x) * this.function(x)

}