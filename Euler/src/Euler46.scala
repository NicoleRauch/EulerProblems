import java.util.Date
object Euler46 {

    def main(args: Array[String]) {
        val time = new Date().getTime()
	  println(euler())
	 
	  println(new Date().getTime() - time)
  }
  
    val primes = Basics.primesBelow(1000000)
    val squares = new Array[Int](100)
    
  def euler() = {
      for(i <- 0 to 99){
        squares(i) = i * i
      }
      
    var number = 9
    var composible = true
    while(composible && number < 10000){
      if(!primes.contains(number)){
        composible = isComposible(number)
      }
      number = number + 2
    }
    println("Zusammensetzbar: " + composible)
    number - 2
  }
    
    def isComposible(number : Int) : Boolean = {
      for(p <- primes){
        if(p < number){
        	val rest = number - p
        	if(rest % 2 == 0){
        	  if(squares.contains(rest / 2)){
        	    return true
        	  }
        	}
        }
      }
      false
    }
}