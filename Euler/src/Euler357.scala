object Euler357 {
	val primes = Euler10.trueSieve(50000000)
	println("Primzahlen sind berechnet")
	
  def main(args: Array[String]) {
    println(euler())
  }

  def euler() = {
    resultList(1)

  }

  def resultList(n: Int): List[Int] = {
    //  if(n > 100000000) Nil
    if (n > 100000) Nil
    else if (check(n, divisorsOf(n)).contains(None)) resultList(n + 1)
    else n :: resultList(n + 1)

  }

  def check(n: Int, l: List[Int]): List[Option[Int]] = {
    l match {
      case Nil => Nil
      case (d :: xs) => { val p = checkPrime(d + n / d); if (p == None) None :: Nil else p :: check(n, xs) }
    }
  }

  def checkPrime(p: Int): Option[Int] = {
    if (primes.contains(p)) Some(p)
    else None
  }

  def divisorsOf(n: Int) = {
    divisors(n, n)
  }

  def divisors(current: Int, n: Int): List[Int] = {
    current match {
      case 0 => Nil
      case 1 => 1 :: Nil
      case x if (x == n) => x :: divisors(x / 2, n)
      case x if (n % x == 0) => x :: divisors(x - 1, n)
      case x => divisors(x - 1, n)
    }
  }
}