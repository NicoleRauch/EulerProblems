import java.util.Date
object Euler21 {

    def main(args: Array[String]) {
    val time = new Date().getTime()
    println(euler())
    println(new Date().getTime() - time)
  }

    val results = new Array[Int](10000)
    
  def euler() = {
    	for(i<-0 to 9999){
    	  results(i) = 0
    	}
    	
    	for(i <- 1 to 9999){
    	  if(results(i) == 0){
    	    val summe = Basics.properDivisorsOf(i).sum
    	    if(i != summe && Basics.properDivisorsOf(summe).sum == i){
    	    	results(i) = i
    	    	results(summe) = summe 
    	    }
    	  }
    	}
    	println(results.filter(x => x > 0).toList)
    	results.sum
  }
  
}