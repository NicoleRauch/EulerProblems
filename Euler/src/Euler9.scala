object Euler9 {

    def main(args: Array[String]) {
	  println(euler())
  }
  
  def euler() = {
    var list:List[(Int,Int,Int)] = Nil
    for(a <- 1 to 333;
    	b <- a+1 to 1000-a;
    	c <- b+1 to 1000-b-a)
    {
    	list = append(a,b,c,list)
    }
    list.head._1 * list.head._2 * list.head._3
  }
  
  def append(a:Int,b:Int,c:Int, l:List[(Int,Int,Int)]) = {
	  if(a*a + b*b == c*c && a + b + c == 1000) (a,b,c) :: l
	  else l
  }
}