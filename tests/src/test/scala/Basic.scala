import org.scalatest.FunSuite

class BasicSuite extends FunSuite {
	
  test("evaluates 42") {
    assert(ctfe(42) === 42)
  }

  test("evaluates 8") {
    assert(ctfe({val a = 8; {val a = 1000; 0}; a}) == 8)
  }

  test("primitives' methods evaluation") {
    assert(ctfe({val a = 2; val b = 7; a+b}) == 9)
  }

  test("while test") {
    assert(ctfe({var a = 0; while (a < 10) {a += 1;88}; a}) == 10)
  }

  test("if test") {
    assert(ctfe({val a = 10; val b = 5; if (a == b+5) "ok" else "notOk"}) == "ok")
  }

}