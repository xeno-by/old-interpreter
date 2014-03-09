package scala.reflect.interpreter
package internal

trait Errors {
  self: Engine =>
  import u._

  // TODO: later on we should review the errors and figure out an exception hierarchy for them
  // however for now let's not worry much about this, because that's a lot of work constantly rehashing the hierarchy
  // and let's just use sys.error to facilitate rapid experimentation

  def UnobtainableSource(sym: Symbol) = sys.error(s"can't obtain source for $sym")

  def UnattributedAst(culprit: Tree) = sys.error(s"can't interpret unattributed tree $culprit")

  def UnrecognizedAst(culprit: Tree) = sys.error(s"can't interpret unrecognized tree $culprit")

  def UnreifiableResult(value: Value) = sys.error(s"can't reify evaluation result $value")
}