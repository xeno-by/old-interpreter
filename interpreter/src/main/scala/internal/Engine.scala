package scala.reflect.eval
package internal

import scala.reflect.core._

object Interpreter {
  def eval(term: Term): Any = {
    term match {
      case Lit.Bool(value) => value
      case Lit.Int(value) => value
      case Lit.Long(value) => value
      case Lit.Float(value) => value
      case Lit.Double(value) => value
      case Lit.Char(value) => value
      case Lit.String(value) => value
      case Lit.Symbol(value) => value
      case Lit.Null() => null
      case Lit.Unit() => ()
      case Term.Apply(Term.Apply(Term.Name(meth), Lit.String(value) :: Nil), _) if meth == "scala.reflect.core.Lit.String.apply" => Lit.String(value)
      case _ => sys.error(s"""
        |unsupported tree:
        |${term.showCode}
        |${term.showRaw}
      """.trim.stripMargin)
    }
  }
}