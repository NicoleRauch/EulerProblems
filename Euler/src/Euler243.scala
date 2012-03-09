import java.util.Date
object Euler243 {

  val starttime = new Date().getTime()

  def main(args: Array[String]) {
    println(euler())
    println(new Date().getTime() - starttime)
  }

  val primes = Euler10.trueSieve(999999).toList
  val results = new Array[Int](1000000)

  def euler() = {
    //        println(firstDenominatorWithResilianceFractionLessThan(0.4) == 12)
    //        println(firstDenominatorWithResilianceFractionLessThan(0.3) == 30)
    //        println(firstDenominatorWithResilianceFractionLessThan(0.2) == 30030)
    //        println(resilienceCount(30030) == 5760)
    //        println(resilienceCount(100000001) == 94117648)

    val desired = 0.19 // 15499.0 / 94744
    firstDenominatorWithResilianceFractionLessThan(desired)
  }

  def firstDenominatorWithResilianceFractionLessThan(desired: Double) = {
    var d = 1
    while (resilienceCount(d) >= (d - 1) * desired) {
      d = d + 1
      if (d % 1000 == 0) {
        //    	  println(d)
        println(d + " - " + (new Date().getTime() - starttime))
      }
      //      if (d < 0) println("Overflow!")
    }
    d
  }

  def resilienceCount(d: Int) = {
    val factors = Basics.primefactors(d, primes)
    val uniquefactors = unique(factors)
    var count = 1
    var part = 2
    val factor = smallestTwin(factors)
    if (factor != 0) {
      part = d / factor
      count = results(part)
    }
    for (i <- part to d - 1) {
      if (isResilientComparedTo2(i, uniquefactors)) count = count + 1
    }

    //    println(d + " - " + count)
    results(d) = count
    count
  }

  def smallestTwin(factors: List[Int]): Int = {
    factors match {
      case f1 :: f2 :: fs => if (f1 == f2) f1 else smallestTwin(f2 :: fs)
      case _ => 0
    }
  }
  
  def unique(factors : List[Int]) : List[Int] = {
    factors match {
      case f1 :: f2 :: fs => if(f1 == f2) unique(f2::fs) else f1 :: unique(f2::fs)
      case x => x
    }
  }

  def isResilientComparedTo2(x: Int, factors: List[Int]): Boolean = {
    factors match {
      case Nil => true
      case p :: ps => if (x % p == 0) false else isResilientComparedTo2(x, ps)
    }
  }

}