object Euler38 {
  def main(args: Array[String]) {
    println(euler())
  }

  def euler() = {
    
    (for( i <- 1 to 9999; j <- 1 to 5) yield check(i,j)).filter(x => x > 0).max
    
//    Euler32.is9Pandigital()
  }
  
  def check(number:Int, prod:Int) = {
	 val str = (for(i <- 1 to prod) yield (number * i).toString()).foldLeft ("") ((s,i) => s + i)
	 if(Basics.is9Pandigital(str))
	   Integer.parseInt(str)
	  else 0
  }
}