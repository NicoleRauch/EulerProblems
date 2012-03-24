import java.util.Date
object Euler12 {

  def main(args: Array[String]) {
    val time = new Date().getTime()
    println(euler())

    println(new Date().getTime() - time)
  }

  val primes = Basics.primesBelow(9999999).toList // eigentlich zu wenig?!?!

  def euler() = {
    var x = 1
    var triangle = 1L
    var divisorCount = 0.0
    while (divisorCount <= 500) {
      x = x + 1
      triangle = triangle + x

//      val divisors = Basics.properDivisorsOf(triangle)
//      divisorCount = divisors.size + 1
//      println("Divisors of " + triangle + ": " + divisors + " - " + divisorCount)
      
      val factors = Basics.primefactors(triangle, primes)
      divisorCount = correctionFactor(factors) * math.pow(2, Basics.unique(factors).size)
//      println("Primefactors: " + factors + " - " + divisorCount)
    }
    println(x)
    triangle
  }
  
  /*
   * Sind alle Primfaktoren disjunkt, ist die Anzahl der Faktoren einer Zahl gleich der Summe der 
   * Elementzahl aller k-elementigen Teilmengen dieser Primfaktormenge (k = 1 .. Grš§e der Primfaktormenge) + 1 
   * (da 1 in der Menge der Faktoren liegt, aber nicht in der Menge der Primfaktoren).
   * Es gilt der Binomialkoeffizient Summe(k=0 .. n) (n Ÿber k) = 2^n
   * 
   * Sind die Primfaktoren nicht disjunkt, muss man diese Zahl mit einem Korrekturfaktor multiplizieren.
   * Dieser Korrekturfaktor ist 1 fŸr einen einmal vorkommenden Primfaktor und wird mit jedem weiteren Vorkommen dieses
   * Faktors um 0,5 erhšht. 
   * 
   */

  def correctionFactor(factors: List[Int]) = {
    def dupli(currentNumber: Int, currentCount: Double, factors: List[Int]): Double = {
      factors match {
        case Nil => currentCount
        case f :: fs if (currentNumber == f) => dupli(currentNumber, currentCount + 0.5, fs)
        case f :: fs => currentCount * dupli(f, 1, fs)
      }
    }

    factors match {
      case Nil => 1
      case f :: fs => dupli(f, 1, fs)
    }
  }

}

// 100 (384 - 73920) : 2 sek
// 200 (2015 - 2031120) : 14 sek