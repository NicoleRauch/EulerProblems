import scala.collection.immutable.SortedMap
import java.util.Date
object Euler14 {

  val array = new Array[Long](1000000)

  def main(args: Array[String]) {
    val time = new Date().getTime()
    println(euler())
    println(new Date().getTime() - time)
  }

  def euler() = {
    for (i <- 0 to 999999)
      array(i) = 0
    for (i <- 1 to 999999) // 999999
      array(i) = sequence(i, 1)

      var max = 0
      var maxindex = 0
      for(i <- 1 to 999999){
        if(array(i) > max){
          max = array(i).toInt
          maxindex = i
        }
      }
    maxindex
  }

  def sequence(n: Long, count: Long): Long = {
    n match {
      case 1 => count
      case n if (n % 2 == 0) => step(n / 2, count)
      case n => step(3 * n + 1, count)
    }
  }

  def step(n: Long, count: Long): Long = {
    if (n < 1000000 && array(n.toInt) > 0) {
      count + array(n.toInt)
    } else
      sequence(n, count + 1)
  }
}