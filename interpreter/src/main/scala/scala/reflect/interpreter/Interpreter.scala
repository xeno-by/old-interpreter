package scala.reflect.interpreter

// TODO: for now we work with scala-reflect.jar
// later on when reflection core is ready, we'll switch to it
import scala.reflect.macros.Universe
import scala.reflect.macros.blackbox.Context
import internal.Engine
import internal.Emulators

trait InterpreterRequires {
  val u: Universe
  def source(sym: u.Symbol): u.MemberDef
  implicit class RichSymbol(sym: u.Symbol) {
    def source = InterpreterRequires.this.source(sym)
  }
}

trait InterpreterProvides {
  self: InterpreterRequires =>
  def eval(tree: u.Tree): Any
}

object interpret {
  def apply(c: Context)(tree: c.Tree): Any = {
    import c.universe._
    val engine = new {
      val u: c.universe.type = c.universe
    } with Engine with Emulators with InterpreterRequires with InterpreterProvides{
      def source(sym: Symbol): MemberDef = {
        // TODO: general case of obtaining TSTs for symbols isn't implemented yet
        // therefore for now we only support whatever we can find directly in the interpretee
        // later on this function will be provided by interpretation hosts
        // so here, in this project, we should not worry about it
        val result = tree.collect{ case src: MemberDef if src.symbol == sym => src }.headOption
        result.getOrElse(UnobtainableSource(sym))
      }
    }
    engine.eval(tree)
  }
}