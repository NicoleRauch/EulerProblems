import java.util.Date
object Euler34 {

  def main(args: Array[String]) {
    val time = new Date().getTime()
    println(euler())
    println(new Date().getTime() - time)
  }

  val fac = new Array[Int](10)
  val results = new Array[Int](1000000)

  def euler() = {
    fac(0) = 1
    for (i <- 1 to 9) {
      fac(i) = fac(i - 1) * i
    }

    results(3, 9999999).sum
  }

  def results(x: Int, max: Int): List[Int] = {
    if (x == max) Nil
    else {
      val result = x.toString().toCharArray().map(Basics.char2Int).map(x => fac(x)).sum
      if (result == x) result :: results(x + 1, max) else results(x + 1, max)
    }

  }
}