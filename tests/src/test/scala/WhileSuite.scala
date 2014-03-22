import org.scalatest.FunSuite

class WhileSuite extends FunSuite {

  test("while zero iterations") {
    assert(ctfe { val a = 100; var b = 0; while (a < 1) { b = 10 }; b } == 0)
  }

  test("while mutable var") {
    assert(ctfe { var a = 0; while (a < 10) { a = a+1}; a } == 10)
  }

  test("nested while") {
    assert(ctfe { var i = 0; var j = 0; while (i < 10) { while (j < i) {j=j+i}; i=i+1 };i+j } == 25)
  }
  
}
