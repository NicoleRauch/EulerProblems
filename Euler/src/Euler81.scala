import java.util.Date
object Euler81 {

  val starttime = new Date().getTime()

  def main(args: Array[String]) {
    println(euler())
    println(new Date().getTime() - starttime)
  }

  val numbers5 = Array(Array(131, 673, 234, 103, 18),
    Array(201, 96, 342, 965, 150),
    Array(630, 803, 746, 422, 111),
    Array(537, 699, 497, 121, 956),
    Array(805, 732, 524, 37, 331))

//  val numbers = numbers5
//  val max = 4
  // Ergebnis: 2427
  
  val numbers = Euler81Matrix.numbers80
  val max = 79

  val results = new Array[Array[Int]](81)

  def euler() = {
    for (i <- 0 to 80) {
      results(i) = new Array[Int](81)
      for (j <- 0 to 80)
        results(i)(j) = 0
    }

    minimalPathSum(0, 0)
  }

  def minimalPathSum(x: Int, y: Int): Int = {
    (x, y) match {
      case (a, b) if (results(a)(b) > 0) => ()
      case (a, b) if (a == max && b == max) => results(a)(b) = numbers(a)(b)
      case (a, b) if (a == max) => results(a)(b) = numbers(a)(b) + minimalPathSum(a, b + 1)
      case (a, b) if (b == max) => results(a)(b) = numbers(a)(b) + minimalPathSum(a + 1, b)
      case (a, b) => results(a)(b) = numbers(a)(b) + minimalPathSum(a, b + 1).min(minimalPathSum(a + 1, b))
    }
    results(x)(y)
  }

}