import org.scalatest.FunSuite

class OOSuite extends FunSuite {

  test("empty object") {
    assert(ctfe { class A; new A; 42 } == 42)
  }

  test("trait method lookup simple") {
    assert(ctfe { trait A { def f() = 42 }; object B extends A; B.f() } == 42)
  }

  test("inherited method call") {
    assert(ctfe { class A {def f() = 42}; class B extends A; (new B).f()} == 42)
  }

  test("inherited field access") {
    assert(ctfe { class A {val v = 42}; class B extends A; (new B).v } == 42)
  }

  test("multiple field inheritance") {
    assert(ctfe {
      trait A{def a = 10}
      trait B{def b = 30}
      trait C{def c = 2}
      class F extends A with B with C {def f = a+b+c}
      (new F).f
    } == 42)
  }

  test("mutable fields") {
    assert(ctfe {
      class A {var v = 100}
      val a = new A
      a.v = 42
      a.v
    } == 42)
  }

  test("lazy fields") {
    assert(ctfe {
      class A {
        var v = 100
        lazy val a = v
        v = 42
      }
      (new A).a
    } == 42)
  }

  test("field references in initialization") {
    assert(ctfe {
      class A {
        val a = 100
        val b = a
        val c = b - a + 42
      }
      (new A).c
    } == 42)
  }

  test("runtime polymorphism simple") {
    assert(ctfe { class A{def f() = 100}; class B extends A{override def f() = 42}; (new B).f() } == 42)
  }

  test("simple super call") {
    assert(ctfe { class A{def f() = 100}; class B extends A{override def f() = super.f()+42}; (new B).f() } == 142)
  }

  test("polymorphic method call") {
    assert(ctfe {
      class A { def f() = 100}
      class B extends A { override def f() = 42}
      val b: A = new B
      b.f()
    } == 42)
  }

  test("overloaded select") {
    assert(ctfe {
      class A{ def f = 100; def f(a: Int) = a + 100}
      class B extends A{ override def f = 999; override def f(a: Int) = a}
      (new B).f(42)
    } == 42)
  }

  test("polymorphic val") {
    assert(ctfe {
      class A { val v = 99}
      class B extends A { override val v = 42}
      val b: A = new B
      b.v
    } == 42)
  }

  test("runtime polymorphism with a callback") {
    assert(ctfe {
      class A { def f() = 999; def g() = f() + 2}
      class B extends A {override def f() = 40}
      (new B).g()
    } == 42)
  }

  test("constructor simple") {
    assert(ctfe { class A(val v: Int); new A(42).v } == 42 )
  }

  test("constructor default parameters") {
    assert(ctfe { class A(val v: Int, val z: Int = 40){def f = v+z}; new A(2).f } == 42)
  }

  test("constructor initialization order") {
    assert(ctfe { class A(var v: Int){v = 42}; new A(999).v } == 42)
  }

  test("ctor field mapping") {
    assert(ctfe {
      class A(x: Int){def f = x}
      class B(val x: Int)
      new A(40).f + new B(2).x
    } == 42)
  }

  test("multiple parameter list ctor") {
    assert(ctfe { class A(a:Int)(b:Int){def f = a+b}; new A(40)(2).f } == 42)
  }

  test("anonymous class") {
    assert(ctfe {
      trait F {val x: Int}
      val v = new F {val x = 42}
      v.x
    } == 42)
  }

  test("lazy field initialization in constructor") {
    assert(ctfe {
      abstract class A { val x: Int; val y = x}
      class B extends A {lazy val x = 42}
      (new B).y
    } == 42)
  }

  test("anonymous class with early definitions") {
    assert(ctfe {
      trait F {val x: Int}
      val v = new {val x = 42} with F
      v.x
    } == 42)
  }

  test("self type") {
    assert(ctfe {
      trait A {def f = 999}
      trait B extends A {override def f = 22}
      class C {
        self: A =>
        def g = 20 + f
      }
      (new C with B).g
    } == 42)
  }

  test("self type with multiple injections") {
    assert(ctfe {
      trait A {def f = 999}
      trait B extends A {override def f = 22}
      trait K {def k = 888}
      trait M extends K {override def k = 19}
      class C {
        self: A with K=>
        def g = 1 + f + k
      }
      (new C with B with M).g
    } == 42)
  }

  test("class closure") {
    assert(ctfe {
      class A(a: Int) {
        class Inner {def f(v: Int) = a + v}
        def g = (new Inner).f(22)
      }
      new A(20).g
    } == 42)
  }

  test("class inside a function") {
    assert(ctfe {
      class B {val a = 20}
      def f(a: Int) = {
        class A(val v: Int) extends B
        new A(a).v + new A(a).a
      }
      f(22)
    } == 42)
  }

  test("class inside a module") {
    assert(ctfe {
      object Obj {
        val v = 40
        class A{def f = v + 2}
        def f = (new A).f
      }
      Obj.f
    } == 42)
  }
}
