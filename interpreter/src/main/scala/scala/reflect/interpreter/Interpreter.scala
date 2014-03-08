package scala.reflect.interpreter

// TODO: for now we work with scala-reflect.jar
// later on when reflection core is ready, we'll switch to it
import scala.reflect.api.Universe
import scala.reflect.macros.blackbox.Context
import internal.Engine

trait InterpreterRequires {
  val u: Universe
  def body(meth: u.MethodSymbol): u.Tree
}

trait InterpreterProvides {
  self: InterpreterRequires =>
  def eval(tree: u.Tree): Any
}

object interpret {
  def apply(c: Context)(tree: c.Tree): Any = {
    val engine = new {
      val u: c.universe.type = c.universe
    } with Engine with InterpreterRequires with InterpreterProvides {
      def body(meth: u.MethodSymbol): u.Tree = ??? // not implemented yet
    }
    engine.eval(tree)
  }
}