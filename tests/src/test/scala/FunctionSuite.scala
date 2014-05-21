import org.scalatest.FunSuite

class FunctionSuite extends FunSuite{

  // test("function simple") {
  //   assert(ctfe { def f(a: Int) = a+1; f(99) } == 100)
  // }

  // test("recursion simple") {
  //   assert(ctfe {
  //     def f(a: Int): Int = if (a > 0) f(a-1) + a else 1
  //     f(10)
  //   } == 56)
  // }

  // test("function as value") {
  //   assert(ctfe {
  //     def f(a: Int, b: Int) = a+b
  //     def g(f: (Int, Int) => Int, i: Int) = f(i,i)
  //     g(f, 5)
  //   } == 10)
  // }

  // test("lambda simple") {
  //   assert(ctfe {
  //     def g(f:Int => Int, v: Int) = f(v)
  //     g((a:Int) => a, 42)} == 42)
  // }

  // test("lambda value capturing") {
  //   assert(ctfe {
  //     val v = 40
  //     def g(f: (Int, Int) => Int, i: Int) = f(i,i)
  //     g((a: Int, b: Int) => a+b+v, 1)
  //   } == 42)
  // }

  // test("high order functions") {
  //   assert(ctfe {
  //     def g(a: Int) = {
  //       def f(b: Int) = a+b
  //       f _
  //     }
  //     g(40)(2)
  //   } == 42)
  // }

  // test("multiple parameter list simple") {
  //   assert(ctfe { def f(a:Int)(b:Int) = a+b; f(40)(2) } == 42)
  // }

  // test("multiple parameter list currying") {
  //   assert(ctfe {
  //     def f(a:Int)(b:Int) = a+b
  //     val g = f(40)(_)
  //     g(2)
  //   } == 42)
  // }

  // test("zero parameter list") {
  //   assert(ctfe { def f = 42; f } == 42)
  // }

  // test("zero parameter list eval on lookup") {
  //   assert(ctfe { def f = 40; val v = f; v+2 } == 42)
  // }

  // test("return statement simple") {
  //   assert(ctfe {
  //     def f: Int = {
  //       return 42
  //       999
  //     }
  //     f
  //   } == 42)
  // }


  // test("return from while") {
  //   assert(ctfe {
  //     def f: Int = { while (true) { return 42 }; 999 }
  //     f
  //   } == 42)
  // }

  // test("nested return") {
  //   assert(ctfe {
  //     def g: Int = { return 42; 999 }
  //     def f: Int = { return g; 888 }
  //     f
  //   } == 42)
  // }

  // test("return from lambda handled by method") {
  //   assert(ctfe {
  //     def f(g: => Int) = {g; 888}
  //     def k:Int = {f({return 42; 777}); 999}
  //     k
  //   } == 42)
  // }

}
