import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BasicsTest extends FunSuite {

  test("Die Liste aller Primzahlen kleiner einer gegebenen Zahl") {
    expect(Nil) {
      Basics.primesBelow(1)
    }
    expect(Nil) {
      Basics.primesBelow(2)
    }
    expect(List(2)) {
      Basics.primesBelow(3)
    }
    expect(List(2, 3, 5, 7)) {
      Basics.primesBelow(10)
    }
    expect(List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)) {
      Basics.primesBelow(100)
    }
  }

  test("Die Liste aller Primfaktoren einer gegebenen Zahl (ohne vorgegebene Primzahlen)") {
    expect(Nil) {
      Basics.factorsOf(1)
    }
    expect(List(2)) {
      Basics.factorsOf(2)
    }
    expect(List(5)) {
      Basics.factorsOf(5)
    }
    expect(List(2, 5)) {
      Basics.factorsOf(10)
    }
    expect(List(2, 2, 5, 5)) {
      Basics.factorsOf(100)
    }
  }

  test("Die Liste aller Primfaktoren einer gegebenen Zahl (mit vorgegebenen Primzahlen)") {
    val primes = Basics.primesBelow(100)
    expect(Nil) {
      Basics.primefactors(1, primes)
    }
    expect(List(2)) {
      Basics.primefactors(2, primes)
    }
    expect(List(5)) {
      Basics.primefactors(5, primes)
    }
    expect(List(2, 5)) {
      Basics.primefactors(10, primes)
    }
    expect(List(2, 2, 5, 5)) {
      Basics.primefactors(100, primes)
    }
  }

  test("Die Liste der echten Teiler einer gegebenen Zahl") {
    expect(Nil) {
      Basics.properDivisorsOf(1)
    }
    expect(List(1)) {
      Basics.properDivisorsOf(2)
    }
    expect(List(1)) {
      Basics.properDivisorsOf(5)
    }
    expect(List(1, 2, 5)) {
      Basics.properDivisorsOf(10)
    }
    expect(List(1, 2, 4, 5, 10, 20, 25, 50)) {
      Basics.properDivisorsOf(100)
    }
  }

  test("Die Liste der Teiler einer gegebenen Zahl") {
    expect(List(1)) {
      Basics.divisorsOf(1)
    }
    expect(List(1, 2)) {
      Basics.divisorsOf(2)
    }
    expect(List(1, 5)) {
      Basics.divisorsOf(5)
    }
    expect(List(1, 2, 5, 10)) {
      Basics.divisorsOf(10)
    }
    expect(List(1, 2, 4, 5, 10, 20, 25, 50, 100)) {
      Basics.divisorsOf(100)
    }
  }

  test("Die Triangularzahl (1 + ... + n) einer Zahl") {
    expect(1) {
      Basics.triangle(1)
    }
    expect(3) {
      Basics.triangle(2)
    }
    expect(15) {
      Basics.triangle(5)
    }
    expect(55) {
      Basics.triangle(10)
    }
    expect(5050) {
      Basics.triangle(100)
    }
  }

  test("Die Unkürzbarkeit eines Bruchs") {
    val primes = Basics.primesBelow(100)
    assert(Basics.isResilient(2, 4, primes) == false)
    assert(Basics.isResilient(3, 9, primes) == false)
    assert(Basics.isResilient(10, 100, primes) == false)

    assert(Basics.isResilient(1, 4, primes) == true)
    assert(Basics.isResilient(2, 5, primes) == true)
    assert(Basics.isResilient(3, 11, primes) == true)
    assert(Basics.isResilient(3, 14, primes) == true)
    assert(Basics.isResilient(3, 28, primes) == true)
    assert(Basics.isResilient(10, 101, primes) == true)
  }

  test("Die Fakultät einer Zahl") {
    expect(1) {
      Basics.fakultaet(1)
    }
    expect(2) {
      Basics.fakultaet(2)
    }
    expect(6) {
      Basics.fakultaet(3)
    }
    expect(120) {
      Basics.fakultaet(5)
    }
    expect(3628800) {
      Basics.fakultaet(10)
    }
  }

  test("Die Fibonacci-Zahl einer Zahl") {
    expect(1) {
      Basics.fakultaet(1)
    }
    expect(2) {
      Basics.fakultaet(2)
    }
    expect(6) {
      Basics.fakultaet(3)
    }
    expect(120) {
      Basics.fakultaet(5)
    }
    expect(3628800) {
      Basics.fakultaet(10)
    }
  }

  test("Eine sortierte Liste eindeutig machen") {
    expect(Nil) {
      Basics.unique(Nil)
    }
    expect(List(2)) {
      Basics.unique(List(2))
    }
    expect(List(2)) {
      Basics.unique(List(2, 2))
    }
    expect(List(2, 3)) {
      Basics.unique(List(2, 2, 3))
    }
    expect(List(2, 3)) {
      Basics.unique(List(2, 2, 3, 3))
    }
  }

  test("Die Unteilbarkeit einer Zahl durch eine Liste vorgegebener Faktoren") {
    assert(Basics.isNotDivisibleBy(4, List(2)) == false)
    assert(Basics.isNotDivisibleBy(4, List(2, 3)) == false)
    assert(Basics.isNotDivisibleBy(4, List(2, 3, 5)) == false)

    assert(Basics.isNotDivisibleBy(10, Nil) == true)
    assert(Basics.isNotDivisibleBy(10, List(3)) == true)
    assert(Basics.isNotDivisibleBy(10, List(3, 7, 9)) == true)
  }

  test("Überprüfung, ob eine Zahl ein Palindrom ist") {
    assert(Basics.checkPalindrome(1) == true)
    assert(Basics.checkPalindrome(11) == true)
    assert(Basics.checkPalindrome(12) == false)
    assert(Basics.checkPalindrome(12233221) == true)
  }
  
  test("Überprüfung, ob eine Zahl pandigital zur Basis 1..9 ist") {
    assert(Basics.is9Pandigital("391867254") == true)
    assert(Basics.is9Pandigital("192384576") == true)
    assert(Basics.is9Pandigital("918273645") == true)
    assert(Basics.is9Pandigital("112233445") == false)
  }
  
  test("Einen Buchstaben in eine Zahl umwandeln") {
    expect(0) {
      Basics.char2Int('0')
    }
    expect(2) {
      Basics.char2Int('2')
    }
    expect(5) {
      Basics.char2Int('5')
    }
    expect(8) {
      Basics.char2Int('8')
    }
    expect(9) {
      Basics.char2Int('9')
    }
  }

  test("Die Quadratsumme der Ziffern einer Zahl") {
	  expect(0) {
		  Euler92.squareSumOfDigits(0)
	  }
	  expect(145) {
		  Euler92.squareSumOfDigits(89)
	  }
	  expect(1) {
		  Euler92.squareSumOfDigits(1)
	  }
	  expect(32) {
		  Euler92.squareSumOfDigits(44)
	  }
	  expect(13) {
		  Euler92.squareSumOfDigits(32)
	  }
	  expect(10) {
		  Euler92.squareSumOfDigits(13)
	  }
	  expect(1) {
		  Euler92.squareSumOfDigits(10)
	  }
	  expect(89) {
		  Euler92.squareSumOfDigits(85)
	  }
	  expect(42) {
		  Euler92.squareSumOfDigits(145)
	  }
	  expect(20) {
		  Euler92.squareSumOfDigits(42)
	  }
	  expect(4) {
		  Euler92.squareSumOfDigits(20)
	  }
	  expect(16) {
		  Euler92.squareSumOfDigits(4)
	  }
	  expect(37) {
		  Euler92.squareSumOfDigits(16)
	  }
	  expect(58) {
		  Euler92.squareSumOfDigits(37)
	  }
	  expect(89) {
		  Euler92.squareSumOfDigits(58)
	  }
  }

    test("Umwandlung römischer Zahlen") {
	  expect("ABCDIVEIVF") {
		  Euler89.replaceFourEqualDigits("ABCDIIIIEIIIIF")
	  }
	  expect("ABCDXLEFXL") {
		  Euler89.replaceFourEqualDigits("ABCDXXXXEFXXXX")
	  }
	  expect("ABCDCDEF") {
		  Euler89.replaceFourEqualDigits("ABCDCCCCEF")
	  }
	  expect("ABCDIXEIIIIF") {
		  Euler89.replace5plus4by9("ABCDVIVEIIIIF")
	  }
	  expect("ABCDXCEFXXXX") {
		  Euler89.replace5plus4by9("ABCDLXLEFXXXX")
	  }
	  expect("ABCDCMEF") {
		  Euler89.replace5plus4by9("ABCDDCDEF")
	  }
	  // echte Zahlen
	  expect("XVI") {
		  Euler89.minimalize("XVI")
	  }
	  expect("XVI") {
		  Euler89.minimalize("VVVI")
	  }
	  expect("XIX") {
		  Euler89.minimalize("VVVIIII")
	  }
    }

}

