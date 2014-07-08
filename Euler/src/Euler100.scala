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

    // und jetzt ohne die gro§en Zahlen:
        // ((e + x) / (e + y)) * ((e + x-1) / (e + y-1)) = 1/2
    // <==>
    // 2 * (e + x) * (e + x-1) = (e + y) * (e + y-1)

    // Sei x = w * y

    // 2 * (e + wy) * (e + wy-1) = (e + y) * (e + y-1)
	// <==> 2 * (e^2 + ewy + ewy - e + w^2y^2 - wy) = e^2 + ey + ey - e + y^2 - y
	  // <==> 2e^2 + 2ewy + 2ewy - 2e + 2w^2y^2 - 2wy = e^2 + ey + ey - e + y^2 - y
	  // <==> 2e^2 + 4ewy - 2e + 2w^2y^2 - 2wy = e^2 + 2ey - e + y^2 - y
	  // <==> e^2 + 4ewy - e + 2w^2y^2 - 2wy - 2ey - y^2 + y = 0
	  // <==> (2y^2) w^2 + (4e - 2) y w + (e^2 - 2ey - e - y^2 + y) = 0
	  // <==> (2y^2) w^2 + (4e - 2) y w + (e^2 - e - 2ey - y^2 + y) = 0

    
    pruefeAlleVonBis(1, e12)
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

  val e12 = 1000000000000L
  val faktor1 = 4 * e12 - 2
  val summand1 = e12 * e12 - e12
  


  def wahrscheinlichkeitIstEinhalb(anzahl_blaue: Long, gesamtzahlProdukt: Long) = {
    2 * anzahl_blaue * (anzahl_blaue - 1) == gesamtzahlProdukt
  }

  private def pruefeAlleVonBis(start: Long, end: Long) = {
    var aktuelleAnzahl = start
    while (aktuelleAnzahl < end) {
//      println(aktuelleAnzahl)
      val anzahlHoch2 = aktuelleAnzahl * aktuelleAnzahl
      val naeherung = Basics.nullstelle(0.00001, (w => 2 * anzahlHoch2 * w * w + faktor1 * aktuelleAnzahl * w 
          + summand1 - 2 * e12 * aktuelleAnzahl - anzahlHoch2 + aktuelleAnzahl), 0.6666, 0.75).intValue() * (aktuelleAnzahl + e12)
          println(naeherung)

      val produkt = (e12 + aktuelleAnzahl) * (e12 + aktuelleAnzahl - 1)
      // a * (a - 1) = a * a - a
      // (a+1) * (a+1-1) = (a+1) * a = a * a + a

      pruefeUndGibAus(produkt, naeherung)
      pruefeUndGibAus(produkt, naeherung + 1)
      aktuelleAnzahl = aktuelleAnzahl + 1
    }
  }

  private def pruefeUndGibAus(gesamtzahlProdukt: Long, anzahl_blaue: Long): Unit = {
    if (wahrscheinlichkeitIstEinhalb(anzahl_blaue, gesamtzahlProdukt)) {
      println("Anzahl blaue Scheiben: " + anzahl_blaue)
    }
  }
}