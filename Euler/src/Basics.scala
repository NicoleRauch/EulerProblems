import scala.annotation.tailrec
object Basics {

  def main(args: Array[String]) {
    print("Die ersten 12 Fibonacci-Zahlen: ")
    for (i <- 1 to 12)
      print(fibonacci(i) + ", ")
    println("")

    //    println(properDivisorsOf(220))
    //    println(properDivisorsOf(284))
  }

  def primesBelow(max: Int): List[Int] = {
    if (max < 2) {
      return Nil
    }
    val primes = new Array[Boolean](max)
    primes(0) = false
    primes(1) = false
    for (i <- 2 to max - 1)
      primes(i) = true

    for (i <- 2 to max - 1; j <- i to max / i) {
      if (i * j < max)
        primes(i * j) = false
    }
    (for (i <- 2 to max - 1) yield if (primes(i)) i else 0).filter(x => x > 0).toList
  }

  /* Primfaktorzerlegung ohne Liste von Primzahlen */
  def factorsOf(x: Long) = {
    def factors(f: Long, x: Long): List[Long] = {
      if (x < 2) Nil
      else if (f * f > x) x :: Nil
      else if (x % f == 0) f :: factors(f, x / f)
      else factors(f + 1, x)
    }
    factors(2, x)
  }

  /* Liste der echten Teiler von x */
  def properDivisorsOf(x: Long) = {
    def divisors(f: Int, x: Long): List[Int] = {
      if (f == x) Nil
      else if (x % f == 0) f :: divisors(f + 1, x)
      else divisors(f + 1, x)
    }
    if (x <= 0) Nil
    else divisors(1, x)
  }

  def divisorsOf(x: Long) = {
    properDivisorsOf(x) ++ (x :: Nil)
  }

  /* Primfaktorzerlegung mit Liste von Primzahlen */
  def primefactors(x: Long, primes: List[Int]): List[Int] = {
    if (x < 2) Nil
    else primes match {
      case p :: ps if (x % p == 0) => p :: primefactors(x / p, p :: ps)
      case p :: ps => primefactors(x, ps)
      case Nil => Nil
    }
  }

  def triangle(x: Long): Long = {
    x match {
      case 1 => 1
      case i => i + triangle(i - 1)
    }
  }

  def isResilient(x: Int, y: Int, primes: List[Int]) = {
    if (x % 2 == 0 && y % 2 == 0)
      false
    else {
      Basics.primefactors(x, primes).intersect(Basics.primefactors(y, primes)).isEmpty
    }
  }

  def fakultaet(n: Int) = {
    (1 to n).foldLeft(1L)((x: Long, y) => x * y)
  }

  def fibonacci2(x: Int): List[Int] = {
    x match {
      case 1 => List(1)
      case 2 => List(2,1)
      case n => 
        fibonacci2(n-1) match {
          case a::b::as => List(a+b, a)
          case _ => List()
        }
    }
  }
  
  def fibonacci(x: Int): Int = {
    x match {
      case 1 => 1
      case 2 => 2
      case n => fibonacci(n - 1) + fibonacci(n - 2)
    }
  }

  def unique(factors: List[Int]): List[Int] = {
    factors match {
      case f1 :: f2 :: fs => if (f1 == f2) unique(f2 :: fs) else f1 :: unique(f2 :: fs)
      case x => x
    }
  }

  def isNotDivisibleBy(x: Long, factors: List[Int]): Boolean = {
    factors match {
      case Nil => true
      case p :: ps => if (x % p == 0) false else isNotDivisibleBy(x, ps)
    }
  }

  def checkPalindrome(x: Int) = {
    val s = x.toString()
    s.reverse == s
  }

  def is9Pandigital(str: String): Boolean = {
    str.length() == 9 && str.contains('1') && str.contains('2') && str.contains('3') && str.contains('4') && str.contains('5') && str.contains('6') && str.contains('7') && str.contains('8') && str.contains('9')
  }
  
  def char2Int(ch:Char) = ch - 48
  
  def steigung (f : Double => Double, x1 : Double, x2 : Double) = ((f (x1)) - (f (x2))) / (x1 - x2)

  def sekantenschritt (f : Double => Double, x1 : Double, x2 : Double) = x1 - (f (x1)) / (steigung (f, x1, x2))

  def nullstelle( genauigkeit : Double, f : Double => Double, x1 : Double, x2 : Double) : Double = {
          val erg = sekantenschritt( f, x1, x2)
          // println(erg + " = f " + x1 + " " + x2)
          if (erg.equals(Double.NaN) || Math.abs (f (erg)) < genauigkeit) erg else nullstelle (genauigkeit, f, x2, erg)
  }

}