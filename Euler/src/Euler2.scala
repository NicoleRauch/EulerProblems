object Euler2 {

  def main(args: Array[String]) {
	  println(euler2())
  }
  
  def euler2() = {
    fibsBelow4Million(1).filter(x => x%2 == 0).sum
  }
  
  def fibsBelow4Million(x:Int) : List[Int] = {
    val fib = Basics.fibonacci2(x).head
    if(fib <= 4000000) fib :: fibsBelow4Million(x+1) 
    else Nil
  }
}