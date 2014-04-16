import org.scalatest.FunSuite

class OOSuite extends FunSuite {

  test("trait method lookup simple") {
    assert(ctfe { trait A { def f() = 42 }; object B extends A; B.f() } == 42)
  }

  test("inherited method call") {
    assert(ctfe { class A {def f() = 42}; class B extends A; (new B).f()} == 42)
  }

  test("inherited field access") {
    assert(ctfe { class A {val v = 42}; class B extends A; (new B).v } == 42)
  }

  test("mutable fields") {
    assert(ctfe {
      class A {var v = 100}
      val a = new A
      a.v = 42
      a.v
    } == 42)
  }

  test("runtime polymorphism simple") {
    assert(ctfe { class A{def f() = 100}; class B extends A{def f() = 42}; (new B).f() } == 42)
  }

  test("runtime polymorphism with a callback") {
    assert(ctfe {
      class A { def f() = 999; def g() = f() + 2}
      class B extends A {override def f() = 40}
      (new B).g()
    } == 42)
  }

}
