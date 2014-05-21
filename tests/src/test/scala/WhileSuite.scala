import org.scalatest.FunSuite

class WhileSuite extends FunSuite {

  // test("while zero iterations") {
  //   assert(ctfe { val a = 100; var b = 0; while (a < 1) { b = 10 }; b } == 0)
  // }

  // test("while mutable var") {
  //   assert(ctfe { var a = 0; while (a < 10) { a = a+1}; a } == 10)
  // }

  // test("nested while") {
  //   assert(ctfe { var i = 0; var j = 0; while (i < 10) { while (j < i) {j=j+i}; i=i+1 };i+j } == 25)
  // }

  // test("side effects in condition") {
  //   assert(ctfe { var v = 0; while({v = 100; false}) {}; v } == 100)
  // }

  // test("do while simple") {
  //   assert(ctfe { var v = 0; do {v = v+1} while (v<0); v } == 1)
  // }

}
