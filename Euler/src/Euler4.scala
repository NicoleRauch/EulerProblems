object Euler4 {

  def main(args: Array[String]) {
	  println(euler4())
  }
  
  def euler4() = {
    
    (for(i <- 100 to 999;j <- 100 to 999) yield i * j).filter(x => checkPalindrome (x)).max
    
   // checkPalindrome(901209)
  }
  
  def checkPalindrome(x:Int) = {
    val s = x.toString()
    s.reverse == s
  }
}

