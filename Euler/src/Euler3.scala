object Euler3 {

  def main(args: Array[String]) {
	  println(euler3())
  }
  
  def euler3() = {
    factors(2,8)
    factors(2, 13195)
    factors(2, 600851475143L)
  }
  
  def factors(f:Int, x:Long) : List[Int] = {
    if(x<2) Nil
    else if(x%f == 0) f :: factors(f,x/f)
    else factors(f+1,x)
  }
}