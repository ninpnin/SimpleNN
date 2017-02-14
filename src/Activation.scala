/**
  * Created by vaino on 13.2.2017.
  */
import scala.math.exp
abstract class Activation {

  def function(x: Double): Double
  def derivative(x: Double): Double

}

object Sigmoid extends Activation {

  def function(x: Double) = 1.0 / (1 + exp(-x))
  def derivative(x: Double) = exp(x) / ((1 + exp(x)) * (1 + exp(x)))

}