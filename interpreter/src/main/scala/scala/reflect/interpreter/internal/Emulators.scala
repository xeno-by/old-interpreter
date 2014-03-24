package scala.reflect.interpreter
package internal

trait Emulators {
  self: Engine =>

  import u._

  trait PrimitivesEmulator {
    def selectCallable(value: Value, sym: Symbol, env: Env): EmulatedCallableValue
  }

  trait ReflectionEmulator extends PrimitivesEmulator {
    def selectCallable(value: Value, sym: Symbol, env: Env): EmulatedCallableValue = ???
  }

  trait ExplicitEmulator extends PrimitivesEmulator {

    val INT_PLUS_FLOAT = selectMethod[Int, Float]("$plus")
    val INT_PLUS_INT = selectMethod[Int, Int]("$plus")
    val INT_MINUS_INT = selectMethod[Int, Int]("$minus")
    val INT_LESS_INT = selectMethod[Int, Int]("$less")
    val INT_GT_INT = selectMethod[Int, Int]("$greater")
    val INT_EQEQ_INT = selectMethod[Int, Int]("$eq$eq")

    def selectCallable(value: Value, sym: Symbol, env: Env): EmulatedCallableValue = {
      def binOp[T, K](receiver: Value, args: List[Value], e: Env, f: (T, K) => Any): Any = {
        f(receiver.reify(e).get.asInstanceOf[T], args.head.reify(e).get.asInstanceOf[K])
      }

      def wrap(v: Any, e: Env) = Value.reflect(v, e)

      val f = (args: List[Value], envf: Env) => wrap(sym match {
        case INT_PLUS_INT    => binOp[Int, Int](value, args, envf, _ + _)
        case INT_MINUS_INT   => binOp[Int, Int](value, args, envf, _ - _)
        case INT_LESS_INT    => binOp[Int, Int](value, args, envf, _ < _)
        case INT_GT_INT      => binOp[Int, Int](value, args, envf, _ > _)
        case INT_EQEQ_INT    => binOp[Int, Int](value, args, envf, _ == _)
        case INT_PLUS_FLOAT  => binOp[Int, Float](value, args, envf, _ + _)
        case other => UnsupportedEmulation(sym)
      }, envf)

      new EmulatedCallableValue(f)
    }

    def selectMethod[T: TypeTag, K: TypeTag](methodName: String):Symbol = {
      u.symbolOf[T].info.member(TermName(methodName)).alternatives.find(m =>
        m.asMethod.paramLists.head.head.typeSignature == u.typeOf[K]).get
    }
  }

}

