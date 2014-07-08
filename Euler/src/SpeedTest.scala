import java.util.Date
object SpeedTest {

    def main(args: Array[String]) {
//      println(5*7*9*11*13*17*23*41L)
//      println(3*5*7*9*11*13*17*23*41L)
//      println(2*3*5*7*9*11*13*17*23*41L)
//      
//      val zahl : Long = 2*3*5*7*9*11*13*17*23*41L*73
//      println(zahl)
      println(Integer.MAX_VALUE)
    val time = new Date().getTime()
	  println(Basics.factorsOf(Integer.MAX_VALUE))
	 
	  println(new Date().getTime() - time)

//	  val time = new Date().getTime()
//	  val primes = Basics.primesBelow(10000000)
//	  println(Basics.primefactors(zahl, primes))
//	  
//	  println(new Date().getTime() - time)
  }
  
}