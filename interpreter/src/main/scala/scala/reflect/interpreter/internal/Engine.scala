package scala.reflect.interpreter
package internal

abstract class Engine extends InterpreterRequires with Errors {
  import u._
  import definitions._

  def eval(tree: Tree): Any = 42
}