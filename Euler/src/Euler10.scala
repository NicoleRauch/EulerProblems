import java.util.Date
object Euler10 {

  def main(args: Array[String]) {
        val time = new Date().getTime()
	  println(euler())
	 
	  println(new Date().getTime() - time)
  }
  
  def euler() = {
    Basics.primesBelow(2000000).foldLeft(0L)((x:Long,y:Int) => x + y)
  }
  

}