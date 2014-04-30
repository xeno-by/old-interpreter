import org.scalatest.FunSuite

class MatchSuite extends FunSuite {

  test("match simple") {
    assert(ctfe { 3 match {
      case 1 => 99
      case 2 => 100
      case 3 => 42
      case _ => 999
    }} == 42)
  }

  test("match simple with guard") {
    assert(ctfe {
      1 match {
        case 0 if 1 == 0 => 999
        case 1 if 1 == 0 => 888
        case 1 if 1 == 1 => 42
        case _ if 1 == 1 => 777
      }
    } == 42)
  }

}
