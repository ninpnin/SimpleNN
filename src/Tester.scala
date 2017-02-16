/**
  * Created by vaino on 13.2.2017.
  */

/* */
object Tester extends App {

  println("LOGISTIC")
  val nn = new Neural(Sigmoid, (2,4,2))
  val data = DataCreator.getData("data.txt")
  nn.optimize(data, 120, 0.0004, 0.00004)

  println("SOFTPLUS")
  val nn2 = new Neural(Softplus, (2,4,2))

  nn2.optimize(data, 120, 0.0004, 0.00004)

  println("TANH")
  val nn3 = new Neural(Tanh, (2,4,2))

  nn3.optimize(data, 120, 0.002, 0.00008)



}
