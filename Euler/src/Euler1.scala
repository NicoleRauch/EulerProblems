object Euler1 {

  def main(args: Array[String]) {
	  println(euler1())
  }
  
  def euler1() = {
    (1 to 999).filter(x => x % 3 == 0 || x % 5 == 0).sum
  }
}