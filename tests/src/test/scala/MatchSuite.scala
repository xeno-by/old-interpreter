import org.scalatest.FunSuite

class MatchSuite extends FunSuite {

  // test("match simple") {
  //   assert(ctfe { 3 match {
  //     case 1 => 99
  //     case 2 => 100
  //     case 3 => 42
  //     case _ => 999
  //   }} == 42)
  // }

  // test("match simple with guard") {
  //   assert(ctfe {
  //     1 match {
  //       case 0 if 1 == 0 => 999
  //       case 1 if 1 == 0 => 888
  //       case 1 if 1 == 1 => 42
  //       case _ if 1 == 1 => 777
  //     }
  //   } == 42)
  // }

  // test("typed match on primitives") {
  //   assert(ctfe {
  //     val v: Any = 1
  //     v match {
  //       case _: Double => 999
  //       case _: Int    => 42
  //       case _         => 888
  //     }
  //   } == 42)
  // }

  // test("direct typed match on classes") {
  //   assert(ctfe {
  //     class A
  //     class B
  //     new B match {
  //       case _: A => 999
  //       case _: B => 42
  //       case _    => 888
  //     }
  //   } == 42)
  // }

  // test("typed match on classes with inheritance") {
  //   assert(ctfe {
  //     class A
  //     class B extends A
  //     val v1 = new B match {
  //       case _: A => 20
  //       case _: B => 999
  //       case _    => 888
  //     }
  //     val v2 = new B match {
  //       case _: B => 22
  //       case _: A => 999
  //       case _    => 888
  //     }
  //     v1+v2
  //   } == 42)
  // }

}
