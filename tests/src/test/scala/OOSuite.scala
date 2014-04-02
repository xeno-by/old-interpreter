import org.scalatest.FunSuite

class OOSuite extends FunSuite {

  test("trait method lookup simple") {
    assert(ctfe { trait A { def f() = 42 }; object B extends A; B.f() } == 42)
  }

}
