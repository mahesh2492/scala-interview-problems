package numbers

import scala.util.Random

object ApproximatePi extends App {

  //compute pi using monte carlo
  def approximatePi(nPoints: Int): Double = {
    val random = new Random(System.currentTimeMillis())
    val nPointsInsideCircle = (1 to nPoints).map { _ =>
      val x = random.nextDouble()
      val y = random.nextDouble()
      x * x + y * y
    }.count(_ < 1)

    nPointsInsideCircle * 4.0 / nPoints
  }

  println(approximatePi(100))
  println(approximatePi(1000))
  println(approximatePi(10000))
  println(approximatePi(100000))
  println(approximatePi(1000000))
  println(approximatePi(10000000))
}
