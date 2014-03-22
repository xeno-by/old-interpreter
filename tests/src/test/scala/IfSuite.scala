import org.scalatest.FunSuite

class IfSuite extends FunSuite {

  test("if test then simple") {
    assert(ctfe { val a = 1; if (a == 1) true else false })
  }

  test("if test else simple") {
    assert(!ctfe { val a = 1; if (a == 999) true else false })
  }

  test("if test with then blocks") {
    assert(ctfe { val a = 1; if (a == 1) { a+1; { a+2; {a+3}}} else 0 } == 4)
  }

  test("if test with else blocks") {
    assert(ctfe { val a = 1; if (a == 100) 0 else { a+1; { a+2; {a+3}}}  } == 4)
  }

  test("nested if simple") {
    assert(ctfe { val a = 1; if (a == 1) if (a+1 == 2) if (a+2 == 3) 999 else 888 else 777 } == 999)
  }

  test("if and mutable values") {
    assert(ctfe { var a = 1; if (a == 1) {a = 2; if (a == 2) {a = 3; a}} } == 3)
  }

}
