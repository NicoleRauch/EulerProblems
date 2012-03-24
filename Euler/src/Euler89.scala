import java.util.Date
object Euler89 {

    def main(args: Array[String]) {
    val time = new Date().getTime()
    println(euler())

    println(new Date().getTime() - time)
  }

  def euler() = {
    var saved = 0
    for(roman <- Euler89List.romans){
      var minimal = minimalize(roman)
      println(minimal)
      saved = saved + roman.length() - minimal.length()
    }
    saved
  }
  
  def minimalize(roman : String) = {
    replace5plus5by10(replace5plus4by9(replace4plus1by5(replaceFourEqualDigits(roman))))
  }
  
  def replaceFourEqualDigits(roman : String) = {
    roman.replaceAll("IIII", "IV").replaceAll("XXXX", "XL").replaceAll("CCCC", "CD")
  }
  
  def replace5plus4by9(roman : String) = {
    roman.replaceAll("VIV", "IX").replaceAll("LXL", "XC").replaceAll("DCD", "CM")
  }

  def replace4plus1by5(roman : String) = {
	  roman.replaceAll("IVI", "V").replaceAll("XLX", "L").replaceAll("CDC", "D")
  }
  
  def replace5plus5by10(roman : String) = {
	  roman.replaceAll("VV", "X").replaceAll("LL", "C").replaceAll("DD", "M")
  }
}