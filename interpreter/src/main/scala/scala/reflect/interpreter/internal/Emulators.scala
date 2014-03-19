package scala.reflect.interpreter
package internal

trait Emulators {
  self: Engine =>

  import u._

  trait PrimitivesEmulator {
    def selectCallable(value: Value, sym: Symbol, env: Env): CallableValue
  }

  trait ReflectionEmulator extends PrimitivesEmulator {
    def selectCallable(value: Value, sym: Symbol, env: Env): CallableValue = ???
  }

  trait ExplicitEmulator extends PrimitivesEmulator {

    val INT_PLUS_INT = selectMethod[Int, Int]("$plus")
    val INT_PLUS_FLOAT = selectMethod[Int, Float]("$plus")
    val INT_LESS_INT = selectMethod[Int, Int]("$less")
    val INT_EQEQ_INT = selectMethod[Int, Int]("$eq$eq")

    def selectCallable(value: Value, sym: Symbol, env: Env): CallableValue = {
      def binOp[T, K](receiver: Value, args: List[Value], f: (T, K) => Any): Any = {
        f(receiver.reify(env).get.asInstanceOf[T], args.head.reify(env).get.asInstanceOf[K])
      }

      def wrap(v: Any, e: Env) = Value.reflect(v, e)

      val f = (args: List[Value], env: Env) => wrap(sym match {
        case INT_PLUS_INT    => binOp[Int, Int](value, args, _ + _)
        case INT_PLUS_FLOAT  => binOp[Int, Float](value, args, _ + _)
        case INT_LESS_INT    => binOp[Int, Int](value, args, _ < _)
        case INT_EQEQ_INT    => binOp[Int, Int](value, args, _ == _)
        case other => UnsupportedEmulation(sym)
      }, env)

      new CallableValue(f)
    }

    def selectMethod[T: TypeTag, K: TypeTag](methodName: String):Symbol = {
      u.symbolOf[T].info.member(TermName(methodName)).alternatives.find(m =>
        m.asMethod.paramLists.head.head.typeSignature == u.typeOf[K]).get
    }
  }

}

