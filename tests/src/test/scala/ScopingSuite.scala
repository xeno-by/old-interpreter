import org.scalatest.FunSuite

class ScopingSuite extends FunSuite {

  test("simple val test") {
    assert(ctfe { val a = 1; a} == 1)
  }

  test("simple var test") {
    assert(ctfe { var a = 1; a } == 1)
  }

  test("var mutability") {
    assert(ctfe { var a = 1; a = 999; a } == 999)
  }

  test("var scoped mutability") {
    assert(ctfe { var a = 100; { a = 99; { a = 88 }}; a } == 88)
  }

  test("val shadowing") {
    assert(ctfe { val a = 1; {val a = 101; {val a = 99 }}; a } == 1)
  }
  
  test("var mutability and shadowing") {
    assert(ctfe { var a = 100; { a = 99; {var a = 77; { a = 66 }}}; a = 88; a } == 88)
  }

  test("val/var cross shadowing") {
    assert(ctfe { val a = 100; {var a = 99; {val a = 88 }}; a } == 100)
  }

}
