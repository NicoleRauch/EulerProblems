import java.util.Date
object Euler27 {

  def main(args: Array[String]) {
    val time = new Date().getTime()
    println(euler())
    println(new Date().getTime() - time)
  }

  val primes = Basics.primesBelow(500000)

  def euler() = {
    // numberOfPrimes(0,1,41) == 40
    // numberOfPrimes(0,-79,1601) == 80

    val result = findMaxResult(for (a <- -999 to 999) yield maxNumberOfPrimes(a))
    println(result)
    result._2 * result._3
    //    maxNumberOfPrimes(-999)
  }

  def maxNumberOfPrimes(a: Int) = {
    findMaxResult(for (b <- -999 to 999) yield (numberOfPrimes(0, a, b),a,b))
  }
  
  def findMaxResult(resultlist : IndexedSeq[(Int,Int,Int)]) = {
        var max=0
    var result = (0,0,0)
    for(i <- resultlist){
      if(i._1 > max){
        max = i._1
        result = i
      }
    }
    result

  }

  // minimal langsamere Lšsung:
  //  def numberOfPrimesIt(a:Int, b:Int) = {
  //    var goOn = true
  //    var n = 0
  //    while(goOn){
  //      goOn = primes contains quad(n,a,b)
  //      n = n + 1
  //    }
  //    n-1
  //  }

  def numberOfPrimes(n: Int, a: Int, b: Int): Int = {
    val result = quad(n, a, b)
    if (result > 1 && result % 2 != 0 && result % 3 != 0 && result % 5 != 0 &&
      result % 7 != 0 && result % 11 != 0 && result % 13 != 0 &&
      (primes contains result)) {
      1 + numberOfPrimes(n + 1, a, b)
    } else 0
  }

  def quad(n: Int, a: Int, b: Int) = {
    n * (n + a) + b
  }
}