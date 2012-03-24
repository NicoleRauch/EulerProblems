object Euler32 {

  def main(args: Array[String]) {
    println(euler())
  }

  def euler() = {

    val result = (for(i <- 1 to 9999;
    j <- 1 to 999)
    	yield if(isPandigital(i, j, i*j)) i*j else 0).filter(x => x > 0)
    (Set() ++ result).sum
  }

  def isPandigital(p1: Int, p2: Int, p3: Int) : Boolean = {
    val str = p1.toString() + p2.toString() + p3.toString()
    Basics.is9Pandigital(str)
  }
  
}