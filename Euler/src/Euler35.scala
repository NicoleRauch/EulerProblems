object Euler35 {

  def main(args: Array[String]) {
    println(euler())
  }

  def euler() = {
    val primes = Euler10.trueSieve(999999)

    (for (p <- primes) yield if (isCircular(p, primes)) p else 0).filter(x => x > 0).size
  }

  def isCircular(p: Int, primes: IndexedSeq[Int]) = {
    val pstr = p.toString()
    if (pstr.length() == 1) {
      true
    } else if (pstr.contains('2') || pstr.contains('4') || pstr.contains('6')
      || pstr.contains('8') || pstr.contains('0')) {
      false
    } else {
      var result = true
      var rotated = pstr
      for (i <- 1 to pstr.length()-1) {
        if (result) {
          rotated = rotated.tail ++ (rotated.head :: Nil)
          result = primes.contains(Integer.parseInt(rotated))
        }
      }
      result
    }
  }
}