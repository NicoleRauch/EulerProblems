import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class Euler4Test extends FunSuite {

  test("Palindrom-Checker") {
    
    assert(Euler4.checkPalindrome(1) == true)
    assert(Euler4.checkPalindrome(11) == true)
    assert(Euler4.checkPalindrome(12) == false)
  }

}