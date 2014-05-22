import scala.language.experimental.macros
import scala.reflect.core._
import scala.reflect.semantic._
import scala.reflect.internal.eval._
import errors.throwExceptions

object ctfe {
  macro apply[T](x: T): T = {
    x.eval match {
      case x: Byte => Lit.Int(x)
      case x: Short => Lit.Int(x)
      case x: Char => Lit.Char(x)
      case x: Int => Lit.Int(x)
      case x: Long => Lit.Long(x)
      case x: Float => Lit.Float(x)
      case x: Double => Lit.Double(x)
      case x: Boolean => Lit.Bool(x)
      case x: Unit => Lit.Unit()
      case x: String => Lit.String(x)
      case _ => c.abort(s"unsupported evaluation result: $x")
    }
  }
}