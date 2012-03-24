import java.util.Date
object Euler100 {

  def main(args: Array[String]) {
    val time = new Date().getTime()
    println(euler())

    println(new Date().getTime() - time)
  }

  def euler() = {

    // Voraussetzung: x <= y
    // x = Anzahl der blauen Scheiben
    // y = Gesamtzahl der Scheiben
    // (x / y) * ((x-1) / (y-1)) = 1/2
    // <==>
    // 2 * x * (x-1) = y * (y-1) = y^2 - y

    // Sei x = w * y

    // y^2 - y = 2 * w y * ( w y - 1) 
    // <==> (y - 1)/2 = w (wy - 1) = y w^2 - w
    // <==> y w^2 - w - (y - 1)/2 = 0

    val y_min = 1000000000001L
//        val y_min = 1000L

    pruefeAlleVonBis(y_min, 1000 * y_min)
  }

  /*
   1.000 - 1.000.000
   2871 # 4060
16731 # 23661
97513 # 137904
568345 # 803761
()
brute force: 954913
2/3 bis 3/4: 82827
nullstelle: 159
   */

  def wahrscheinlichkeit(anzahl_blaue: Long, gesamtzahlProdukt: Long) = {
    2 * anzahl_blaue * (anzahl_blaue - 1) == gesamtzahlProdukt
  }

  private def pruefeAlleVonBis(start: Long, end: Long): Unit = {
    var aktuelleAnzahl = start
    while (aktuelleAnzahl < end) {
      val naeherung = Basics.nullstelle(0.01, (x => aktuelleAnzahl * x * x - x - (aktuelleAnzahl - 1) / 2), 0.6666, 0.75) * aktuelleAnzahl

      val anzahlProdukt = aktuelleAnzahl * (aktuelleAnzahl - 1)
      
      // a * (a - 1) = a * a - a
      // (a+1) * (a+1-1) = (a+1) * a = a * a + a

      pruefeUndGibAus(anzahlProdukt, naeherung.intValue())
      pruefeUndGibAus(anzahlProdukt, naeherung.intValue() + 1)
      aktuelleAnzahl = aktuelleAnzahl + 1
    }
  }

  private def pruefeUndGibAus(gesamtzahlProdukt: Long, anzahl_blaue: Int): Unit = {
    if (wahrscheinlichkeit(anzahl_blaue, gesamtzahlProdukt)) {
      println(anzahl_blaue)
    }
  }
}