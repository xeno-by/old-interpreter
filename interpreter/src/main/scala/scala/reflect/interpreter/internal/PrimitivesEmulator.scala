package scala.reflect.interpreter
package internal

trait Emulators {
  self: Engine with InterpreterRequires =>

  import u._

  trait PrimitivesEmulator {
    def selectCallable(value: Value, sym: Symbol): CallableValue
  }

  trait ReflectionEmulator extends PrimitivesEmulator {
    def selectCallable(value: Value, sym: Symbol): CallableValue = ???
  }

  trait ExplicitEmulator extends PrimitivesEmulator {

    val INT_PLUS_INT = selectMethod[Int, Int]("$plus")
    val INT_PLUS_FLOAT = selectMethod[Int, Float]("$plus")
    val INT_EQEQ_INT = selectMethod[Int, Int]("$eq$eq")

    def selectCallable(value: Value, sym: Symbol): CallableValue = {
      def binOp[T, K](receiver: Value, args: List[Value], f: (T, K) => Any): Any = {
        f(receiver.reify.get.asInstanceOf[T], args.head.reify.get.asInstanceOf[K])
      }

      def wrap(v: Any, e: Env) = Result(JvmValue(v), e)

      val f = sym match {
        case INT_PLUS_INT => (args: List[Value], env: Env) => wrap(binOp[Int, Int](value, args, _ + _), env)
        case INT_PLUS_FLOAT => (args: List[Value], env: Env) => wrap(binOp[Int, Float](value, args, _ + _), env)
        case INT_EQEQ_INT => (args: List[Value], env: Env) => wrap(binOp[Int, Int](value, args, _ == _), env)
        case other => ???
      }

      new CallableValue { override def apply(args: List[Value], env: Env) = f(args, env) }
    }

    def selectMethod[T: TypeTag, K: TypeTag](methodName: String):Symbol = {
      u.symbolOf[T].info.member(TermName(methodName)).alternatives.find(m =>
        m.asMethod.paramLists.head.head.typeSignature == u.typeOf[K]).get
    }
  }

}

