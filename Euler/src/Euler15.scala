object Euler15 {

  def main(args: Array[String]) {
    println(euler())
  }

  val array = new Array[Array[Long]](21)

  def euler() = {
    for (i <- 0 to 20) {
      array(i) = new Array[Long](21)
      for(j <- 0 to 20)
        array(i)(j) = 0
    }

    array(0)(0) = 1
    array(1)(1) = 2
    for (i <- 2 to 20) {
      array(i)(i) = paths(i, i)
    }

    for (i <- 0 to 20) {
      println(i + " = " + array(i)(i))
    }
  }

  def paths(i: Int, j: Int): Long = {
    (i,j) match {
      case (_,0) => 1
      case (2,m) => (m+1)*(m+2)/2
      case (n,m) if (array(n)(m) > 0) => array(n)(m)
      case (n,m) => array(n)(m) = 1 + (for (k <- 1 to m) yield paths(n - 1, k)).sum; array(n)(m)
    }
  }

  def fakultaet(n: Int) = {
    (1 to n).foldLeft(BigInt(1))((x: BigInt, y) => x * y)
  }
}