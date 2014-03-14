import org.scalatest.FunSuite

class BasicSuite extends FunSuite {
	
  test("evaluates 42") {
    assert(ctfe(42) === 42)
  }

  test("evaluates 8") {
    assert(ctfe({var a = 9; {var a = 77}; a = 8; a}) == 8)
  }

  test("primitives' methods evaluation") {
  	assert(ctfe(2+7) == 9)
  }

}