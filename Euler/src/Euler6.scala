object Euler6 {

    def main(args: Array[String]) {
	  println(euler())
  }
  
  def euler() = {
	 val sumOfSquares = (for(i <- 1 to 100) yield i*i).sum
	 val sum = (for(i <- 1 to 100) yield i).sum
	 sum * sum - sumOfSquares

	 // alternative Lšsung:
	 
	 val res = (for(i <- 1 to 100) yield (i*i,i)).foldLeft((0,0)) ((x,y) => (x._1 + y._1,x._2 + y._2))
	 res._2 * res._2 - res._1
  }
}