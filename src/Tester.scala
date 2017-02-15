/**
  * Created by vaino on 13.2.2017.
  */

/* */
object Tester extends App {

  val nn = new Neural(Sigmoid, (2,4,2))

  val data = DataCreator.getData("data.txt")
  nn.optimize(data, 120, 0.0008, 0.00008)

  println("SOFTPLUS")
  val nn2 = new Neural(Softplus, (2,4,2))

  val data2 = DataCreator.getData("data.txt")
  nn.optimize(data, 120, 0.0008, 0.00008)


}
