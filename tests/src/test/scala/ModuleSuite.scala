import org.scalatest.FunSuite

class ModuleSuite extends FunSuite{

  test("object definition") {
    assert(ctfe { object A {}; 0} == 0)
  }

  test("object method") {
    assert(ctfe { object A { def f() = 42}; A.f() } == 42)
  }

  test("object methods") {
    assert(ctfe { object A { def f() = g(); def g() = 42}; A.f() } == 42 )
  }

  test("object fields") {
    assert(ctfe { object A { val v = 42}; A.v } == 42)
  }

  test("object mutable fields") {
    assert(ctfe { object A {var v = 0}; A.v = 42; A.v } == 42)
  }

  test("nested object") {
    assert(ctfe { object A { object B {}}; 0 } == 0)
  }

  test("nested object access") {
    assert(ctfe { object A {object B {val c = 42}}; A.B.c } == 42)
  }

}
