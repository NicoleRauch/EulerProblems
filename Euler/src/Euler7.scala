import java.util.Date
object Euler7 {

      def main(args: Array[String]) {
        val time = new Date().getTime()
	  println(euler())
	  // 104743
	  println(new Date().getTime() - time)
  }
  
  def euler() = {
    buildList(2, Nil)
  }
  
  def buildList(n:Int, l : List[Int]) : Int = {
    val result = sieve(n, l)
    if(result.length == 10001)
      result.last
     else buildList(n+1,result)
  }
  
  def sieve(n:Int, primes:List[Int]) : List[Int] = {
    if(divisibleBy(n, primes)) primes
    else primes ++ (n::Nil)
  }
  
  def divisibleBy(n:Int, primes:List[Int]) : Boolean = {
    primes match {
      case Nil => false
      case p::ps if(n/2 < p) => false
      case p::ps if(n%p == 0)=> true
      case p::ps => divisibleBy(n, ps)
    }
  }
}