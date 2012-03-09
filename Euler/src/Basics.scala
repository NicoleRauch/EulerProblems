object Basics {

    def primefactors(x: Long, primes: List[Int]): List[Int] = {
    if (x < 2) Nil
    else primes match {
      case p :: ps if (x % p == 0) => p :: primefactors(x / p, p :: ps)
      case p :: ps => primefactors(x, ps)
      case Nil => Nil
    }
  }

  def isResilient(x: Int, y: Int, primes: List[Int]) = {
    if (x % 2 == 0 && y % 2 == 0)
      false
    else {
      Basics.primefactors(x, primes).intersect(Basics.primefactors(y, primes)).isEmpty
    }
  }

}