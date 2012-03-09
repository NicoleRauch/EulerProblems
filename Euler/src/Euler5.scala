object Euler5 {

  def main(args: Array[String]) {
	  println(euler())
  }
  
  def euler() = {
	  checkRecursively(20)
  }


  def checkRecursively(x:Int) : List[Int] = {
    val result = check(x)
    if(result == Nil){
      checkRecursively(x+1)
    } else {
      result
    }
  }
  
  def check(x:Int) = {
    filterBy(2, x::Nil)
  }
  
  def filterBy(i : Int, l : List[Int]) : List[Int] = {
	val filtered = l.filter(x => x % i == 0)
    if(i==20 || filtered == Nil){
      filtered
    } else {
      filterBy(i+1, filtered)
    }
  }
  
}
