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
    is9Pandigital(str)
  }
  
  def is9Pandigital(str:String) : Boolean = {
     str.length() == 9 && str.contains('1') && str.contains('2') && str.contains('3')  && str.contains('4') && str.contains('5') && str.contains('6') && str.contains('7') && str.contains('8') && str.contains('9')
  }
}