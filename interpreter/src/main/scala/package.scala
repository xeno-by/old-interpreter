package scala.reflect

import scala.reflect.core._
import scala.reflect.eval.internal.Interpreter

package object eval {
  implicit class EvalOps(val term: Term) extends AnyVal {
    def eval: Any = scala.reflect.eval.eval(term)
  }
  def eval(term: Term): Any = Interpreter.eval(term)
}
