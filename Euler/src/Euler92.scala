import java.util.Date
object Euler92 {

  def main(args: Array[String]) {
    val time = new Date().getTime()
    println(euler())

    println(new Date().getTime() - time)
  }

  def euler() = {
    var countOf89 = 0
    for(i <- 2 to 9999999){
      var result = i
      while(result != 1 && result != 89){
        result = squareSumOfDigits(result)
      }
      if(result == 89){
        countOf89 = countOf89 + 1
      }
    }
    countOf89
  }

  def squareSumOfDigits(x: Int): Int = {
    if (x == 0) {
      0
    } else {
      (x % 10) * (x % 10) + squareSumOfDigits(x / 10)
    }
  }
}