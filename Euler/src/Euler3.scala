object Euler3 {

  def main(args: Array[String]) {
	  println(euler3())
  }
  
  def euler3() = {
    Basics.factorsOf(8)
    Basics.factorsOf(13195)
    Basics.factorsOf(600851475143L)
  }
}