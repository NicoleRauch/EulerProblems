import java.util.Date
object Euler243 {

  val starttime = new Date().getTime()

  def main(args: Array[String]) {
    println(euler())
    println(new Date().getTime() - starttime)
  }

  val primes = Euler10.trueSieve(9999999).toList
  val results = new Array[Int](1000000)

  def euler() = {
    //            println(firstDenominatorWithResilianceFractionLessThan(0.4) == 12)
    //            println(firstDenominatorWithResilianceFractionLessThan(0.3) == 30)
    //            println(firstDenominatorWithResilianceFractionLessThan(0.2) == 30030)
    //            println(resilienceCount(30030) == 5760)

    // 0.1635881955585578
    val desired = 15499.0 / 94744
    firstDenominatorWithResilianceFractionLessThan(desired)

  }
  
  /*

    Das Produkt der ersten 2 Primzahlen: 6
	Das Produkt der ersten 3 Primzahlen: 30
	Das Produkt der ersten 4 Primzahlen: 210
	Das Produkt der ersten 5 Primzahlen: 2310
	Das Produkt der ersten 6 Primzahlen: 30030
	Das Produkt der ersten 7 Primzahlen: 510510
	Das Produkt der ersten 8 Primzahlen: 9699690
	Das Produkt der ersten 9 Primzahlen: 223092870
	
	Das Produkt der ersten 10 Primzahlen: 6469693230
	Das Produkt der ersten 11 Primzahlen: 200560490130
	Das Produkt der ersten 12 Primzahlen: 7420738134810
	Das Produkt der ersten 13 Primzahlen: 304250263527210
	Das Produkt der ersten 14 Primzahlen: 13082761331670030
	Das Produkt der ersten 15 Primzahlen: 614889782588491410

	Damit R(d) klein wird, mŸssen mšglichst viele der BrŸche kŸrzbar sein. D. h. d muss mšglichst
	viele Primfaktoren enthalten. Da d die kleinste Zahl sein soll, suchen wir eine Zahl mit den
	ersten n Primfaktoren.
	Das Ergebnis fŸr 223092870 (mit 9 Primfaktoren) liegt knapp Ÿber dem gesuchten Wert.
	Von hier aus starten wir die Suche.
	Die gesuchte Zahl muss dieselben 9 Primfaktoren enthalten. Da es die kleinste Zahl sein muss,
	multiplizieren wir in jedem Schritt mit 2.

   */

  def firstDenominatorWithResilianceFractionLessThan(desired: Double) = {
    var d = 223092870
    val factors = Basics.primefactors(d, primes)
    val uniquefactors = unique(factors)
    while (resilienceCount(d, uniquefactors) >= (d - 1) * desired) {
      d = d * 2
    }
    d
  }

  def resilienceCount(d: Long, f : List[Int]) = {
      var count = 1
      var i = 2L
      while(i < d){
        if (isResilientComparedTo(i, f)) count = count + 1
        i = i + 1
      }
      count
  }

  def unique(factors: List[Int]): List[Int] = {
    factors match {
      case f1 :: f2 :: fs => if (f1 == f2) unique(f2 :: fs) else f1 :: unique(f2 :: fs)
      case x => x
    }
  }

  def isResilientComparedTo(x: Long, factors: List[Int]): Boolean = {
    factors match {
      case Nil => true
      case p :: ps => if (x % p == 0) false else isResilientComparedTo(x, ps)
    }
  }

}