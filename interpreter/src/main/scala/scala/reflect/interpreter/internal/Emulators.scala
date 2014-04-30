package scala.reflect.interpreter
package internal

trait Emulators {
  self: Engine =>

  import u._

  trait PrimitiveEmulator {
    def selectCallable(value: Value, sym: Symbol, env: Env): EmulatedCallableValue
  }

  trait MagicMethodEmulator extends PrimitiveEmulator {
    def selectCallable(value: Value, sym: Symbol, env: Env) = {
      new EmulatedCallableValue((args: List[Value], env: Env) => {
        def binOp[T1, T2](f: (T1, T2) => Any): Result = {
          val (jvalue, env1) = value.reify(env)
          val arg :: Nil = args
          val (jarg, env2) = arg.reify(env1)
          val jresult = f(jvalue.asInstanceOf[T1], jarg.asInstanceOf[T2])
          Value.reflect(jresult, env2)
        }
        sym match {
          case INT_PLUS_INT     => binOp[Int, Int](_ + _)
          case INT_MINUS_INT    => binOp[Int, Int](_ - _)
          case INT_LESS_INT     => binOp[Int, Int](_ < _)
          case INT_GT_INT       => binOp[Int, Int](_ > _)
          case INT_EQEQ_INT     => binOp[Int, Int](_ == _)
          case INT_PLUS_FLOAT   => binOp[Int, Float](_ + _)
          case Any_equals       => binOp[Any, Any](_.equals(_))
          case other            => UnsupportedEmulation(sym)
        }
      })
    }
  }
}
