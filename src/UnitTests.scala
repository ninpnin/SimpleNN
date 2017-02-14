/**
  * Created by vaino on 13.2.2017.
  */

import collection.mutable.Stack
import org.scalatest._
import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite


//Unit tests for
class UnitTests extends FlatSpec {

  //Set hyper parameters
  val hP = (2,8,2)
  val nn = new Neural(Sigmoid, hP)

  val epsilon = 0.01
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  "Partial differential U" should "be close to delta U" in {
    for (round <- 0 until 10) {

      nn.nullify()
      nn.shuffle()

      val input = Helper.getInput(hP._1)
      val delta = 0.00001
      for (op <- 0 until hP._3) {
        for (i <- 0 until hP._2; j <- 0 until hP._3) {

          val deriv = nn.deltaU(op, i, j, input)
          val before = nn.approx(input)
          nn.WU(i)(j) = nn.WU(i)(j) + delta
          val after = nn.approx(input)
          nn.WU(i)(j) = nn.WU(i)(j) - delta
          val approximation = (after(op) - before(op)) / delta
          assert(approximation === deriv)
          if (deriv != 0.0)
            assert(approximation/deriv === 1.0)

        }
      }
    }
  }

  "Partial differential D" should "be close to delta D" in {
    for (round <- 0 until 10) {

      nn.nullify()
      nn.shuffle()

      val input = Helper.getInput(hP._1)
      val delta = 0.00001
      for (a <- 0 until hP._1; b <- 0 until hP._2; i <- 0 until hP._3) {
        val deriv = nn.deltaD(i, a, b, input)
        val before = nn.approx(input)
        nn.WD(a)(b) = nn.WD(a)(b) + delta
        val after = nn.approx(input)
        nn.WD(a)(b) = nn.WD(a)(b) - delta
        val approximation = (after(i) - before(i)) / delta
        assert(approximation === deriv)
        if (deriv != 0.0)
          assert(approximation/deriv === 1.0)

      }
    }
  }

  val data = DataCreator.getData("data.txt")

  "Partial differential U" should "be close to delta Error" in {
    nn.nullify()
    nn.shuffle()
    val input = Helper.getInput(hP._1)
    val delta = 0.000001
    val deriv = nn.errorGradientU(data)

    for (i <- 0 until hP._2; j <- 0 until hP._3) {

        val before = nn.errorFunction(data)
        nn.WU(i)(j) = nn.WU(i)(j) + delta
        val after = nn.errorFunction(data)
        nn.WU(i)(j) = nn.WU(i)(j) - delta
        val approximation = (after - before ) / delta
        assert(approximation === deriv(i)(j))

    }

  }

  "Partial differential D" should "be close to delta Error" in {
    nn.nullify()
    nn.shuffle()
    val input = Helper.getInput(hP._1)
    val delta = 0.000001
    val deriv = nn.errorGradientD(data)

    for (i <- 0 until hP._1; j <- 0 until hP._2) {
      val before = nn.errorFunction(data)
      nn.WD(i)(j) = nn.WD(i)(j) + delta
      val after = nn.errorFunction(data)
      nn.WD(i)(j) = nn.WD(i)(j) - delta
      val approximation = (after - before ) / delta
      assert(approximation === deriv(i)(j))

    }

  }

}