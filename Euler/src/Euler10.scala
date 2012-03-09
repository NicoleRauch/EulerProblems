import java.util.Date
object Euler10 {

  def main(args: Array[String]) {
        val time = new Date().getTime()
	  println(euler())
	 
	  println(new Date().getTime() - time)
  }
  
  def euler() = {
    trueSieve(2000000).foldLeft(0L)((x:Long,y:Int) => x + y)
  }
  
  def trueSieve(max:Int) = {
    val primes = new Array[Boolean](max)
    primes(0) = false
    primes(1) = false
    for(i <- 2 to max-1)
      primes(i) = true
    
    for(i <- 2 to max-1; j <- i to max/i){
      if(i*j < max)
    	  primes(i*j) = false
    }
    (for(i <- 2 to max-1) yield if(primes(i)) i else 0).filter(x => x > 0)
  }

}