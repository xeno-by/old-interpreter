import org.scalatest.FunSuite

class ValueSuite extends FunSuite {

  // test("val simple") {
  //   assert(ctfe { val a = 42; a } == 42)
  // }

  // test("var simple") {
  //   assert(ctfe { var a = 42; a } == 42)
  // }

  // test("val-val initialisation") {
  //   assert(ctfe { val a = 42; val b = a; b } == 42)
  // }

  // test("val-var initialization") {
  //   assert(ctfe { val a = 42; var b = a; b } == 42)
  // }

  // test("var mutability") {
  //   assert(ctfe { var a = 100; a = 42; a } == 42)
  // }

  // test("transitive immutability") {
  //   assert(ctfe { val a = 42; var b = a; b = 100; a } == 42)
  // }

  // test("lazy val simple") {
  //   assert(ctfe { lazy val a = 42; a} == 42)
  // }

  // test("lazy val evaluation on assign") {
  //   assert(ctfe { lazy val a = 999; var b = a; b = 42; b } == 42)
  // }

  // test("lazy val evaluation on lookup") {
  //   assert(ctfe { var a = 999; lazy val b = a; a = 42; b } == 42)
  // }
}
