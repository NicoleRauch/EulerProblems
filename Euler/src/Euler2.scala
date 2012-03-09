object Euler2 {

  def main(args: Array[String]) {
	  println(euler2())
  }
  
  def euler2() = {
    fibsBelow4Million(1).filter(x => x%2 == 0).sum
  }
  
  def fibsBelow4Million(x:Int) : List[Int] = {
    val fib = fibonacci(x)
    if(fib <= 4000000) fib :: fibsBelow4Million(x+1) 
    else Nil
  }
  
  def fibonacci(x:Int) : Int = {
    x match {
      case 1 => 1
      case 2 => 2
      case n => fibonacci(n-1) + fibonacci(n-2)
    }
  }
}