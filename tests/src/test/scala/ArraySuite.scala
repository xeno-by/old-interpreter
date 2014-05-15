import org.scalatest.FunSuite

class ArraySuite extends FunSuite {

  test("array instantiation simple") {
    assert(ctfe { new Array[Int](1); 42 } == 42)
  }

  test("array element read") {
    assert(ctfe {
      val a = new Array[Int](1)
      a(0) + 42
    } == 42)
  }

  test("array element write and read") {
    assert(ctfe {
      val a = new Array[Int](1)
      a(0) = 42
      a(0)
    } == 42)
  }

  test("array multiple elements") {
    assert(ctfe {
      val a = new Array[Int](3)
      a(0) = 2; a(1) = 10; a(2) = 30;
      a(0) + a(1) + a(2)
    } == 42)
  }

}
