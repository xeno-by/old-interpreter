package scala.reflect.interpreter
package internal

trait Emulators {
  self: Engine =>

  import u._

  trait PrimitiveEmulator {
    def selectCallable(value: Value, sym: Symbol, env: Env): EmulatedCallableValue
  }

  trait MagicMethodEmulator extends PrimitiveEmulator {

    val INT_PLUS_FLOAT = selectMethod[Int, Float]("$plus")
    val INT_PLUS_INT = selectMethod[Int, Int]("$plus")
    val INT_MINUS_INT = selectMethod[Int, Int]("$minus")
    val INT_LESS_INT = selectMethod[Int, Int]("$less")
    val INT_GT_INT = selectMethod[Int, Int]("$greater")
    val INT_EQEQ_INT = selectMethod[Int, Int]("$eq$eq")

    def selectCallable(value: Value, sym: Symbol, env: Env): EmulatedCallableValue = {
      val f = (args: List[Value], env: Env) => {
        def binOp[T, K](f: (T, K) => Any): Result = {
          val (jvalue, env1) = value.reify(env)
          val arg :: Nil = args
          val (jarg, env2) = arg.reify(env1)
          val jresult = f(jvalue.asInstanceOf[T], jarg.asInstanceOf[K])
          Value.reflect(jresult, env2)
        }
        sym match {
          case INT_PLUS_INT     => binOp[Int, Int](_ + _)
          case INT_MINUS_INT    => binOp[Int, Int](_ - _)
          case INT_LESS_INT     => binOp[Int, Int](_ < _)
          case INT_GT_INT       => binOp[Int, Int](_ > _)
          case INT_EQEQ_INT     => binOp[Int, Int](_ == _)
          case INT_PLUS_FLOAT   => binOp[Int, Float](_ + _)
          case other            => UnsupportedEmulation(sym)
        }
      }

      new EmulatedCallableValue(f)
    }

    def selectMethod[T: TypeTag, K: TypeTag](methodName: String):Symbol = {
      u.symbolOf[T].info.member(TermName(methodName)).alternatives.find(m =>
        m.asMethod.paramLists.head.head.typeSignature == u.typeOf[K]).get
    }
  }

}

