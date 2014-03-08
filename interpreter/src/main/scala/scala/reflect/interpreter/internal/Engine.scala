package scala.reflect.interpreter
package internal

trait Engine extends InterpreterRequires {
  def eval(tree: u.Tree): Any = 42
}