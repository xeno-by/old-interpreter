package scala.reflect.interpreter
package internal

import scala.collection.immutable.{ListMap, HashMap}
import scala.collection.mutable

trait Emulators {
  self: Engine =>

  import u._
  import definitions._

  trait PrimitiveEmulator {
    def selectCallable(value: Value, sym: Symbol, env: Env): EmulatedCallableValue
  }

  abstract case class EmulatedCallableValue(f: (List[Value], Env) => Result) extends CallableValue {
    override def apply(args: List[Value], env: Env) = f(args, env)
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
        def unaryOp[T1](f: (T1) => Any): Result = {
          val (jvalue, env1) = value.reify(env)
          val jresult = f(jvalue.asInstanceOf[T1])
          Value.reflect(jresult, env1)
        }
        def dummyOp = Value.reflect((), env)
        sym match {
          case INT_PLUS_INT     => binOp[Int, Int](_ + _)
          case INT_MINUS_INT    => binOp[Int, Int](_ - _)
          case INT_LESS_INT     => binOp[Int, Int](_ < _)
          case INT_GT_INT       => binOp[Int, Int](_ > _)
          case INT_EQEQ_INT     => binOp[Int, Int](_ == _)
          case INT_PLUS_FLOAT   => binOp[Int, Float](_ + _)
          case Any_equals       => binOp[Any, Any](_.equals(_))
          case Any_hashCode     => unaryOp[Any](_.hashCode())
          case Object_hashcode  => unaryOp[java.lang.Object](_.hashCode())
          case Object_init      => dummyOp
          case Throwable_init   => dummyOp
          case other            => UnsupportedEmulation(sym)
        }
      }){ override def isNullary: Boolean = sym.asMethod.paramLists.isEmpty }
    }
  }

  class EmulatedOption(mod :ModuleSymbol) extends TypedValue(mod.typeSignature)
  class EmulatedNone(mod: ModuleSymbol) extends EmulatedOption(mod)
  class EmulatedSomeModule(mod: ModuleSymbol) extends EmulatedOption(mod) {
    class EmulatedSome(tpe: Type, val value: Value, env: Env) extends TypedValue(tpe) {
      override def select(member: Symbol, env: Env, static: Boolean): (Value, Env) = {
        if (member.name.toString == "get") ec((args, e) => (value, e), nullary = false, env)
        else ???
      }
    }
    override def select(member: Symbol, env: Env, static: Boolean): (Value, Env) = {
      if (member.name.decodedName.toString == "apply") 
        ec((args, env) => {(new EmulatedSome(mod.typeSignature, args.head, env), env)}, nullary = false , env)
      else (selectCallable(this, member, env), env)
    }
  }

  class EmulatedArray(tpe: Type) extends TypedValue(tpe) {
    var data: Array[Value] = null
    def constructFrom(v: Array[Value]): EmulatedArray = {data = v; this}
    def constructFrom(t: Type, num: Int, e: Env): Result = {
      val tmp = new mutable.ListBuffer[Result]
      (1 to num).foldLeft(e)((res, _) => (tmp += defaultValue(t, res)).head._2)
      data = tmp.map(_._1).toArray
      (this, tmp.last._2)
    }
    override def select(member: Symbol, env: Env, static: Boolean): (Value, Env) = {
      if (member.isConstructor)
        ec((args, env) => constructFrom(tpe.typeArgs.head, args.head.reify(env)._1.asInstanceOf[Int], env), nullary = false, env)
      else if (member == Array_apply)
        ec((args, env) => (data(args.head.reify(env)._1.asInstanceOf[Int]), env), nullary = false, env)
      else if (member == Array_update) {
        ec((args, env) => {
            data(args.head.reify(env)._1.asInstanceOf[Int]) = args.tail.head
            Value.reflect((), env)
          } , nullary = false, env)
      } else if (member == Array_length)
        ec((args, env) => Value.reflect(data.length, env),nullary = true, env)
      else ???
    }
  }

  class EmulatedArrayModule(mod: ModuleSymbol) extends TypedValue(mod.typeSignature) {
    override def select(member: Symbol, env: Env, static: Boolean): (Value, Env) = {
      if (member == ArrayModule_overloadedApply) (new EmulatedCallableValue(
        (args: List[Value], env: Env) => (new EmulatedArray(mod.typeSignature).constructFrom(args.toArray), env)
      ) {  override def isNullary = false }, env)
      else ???
    }
  }

  private lazy val moduleMappingFactory = HashMap[ModuleSymbol, (ModuleSymbol, Env) => (Value, Env)](
    NoneModule -> {(symbol, env) => (new EmulatedNone(symbol), env)},
    SomeModule -> {(symbol, env) => (new EmulatedSomeModule(symbol), env)},
    ArrayModule-> {(symbol, env) => (new EmulatedArrayModule(symbol), env)}
  )

  private lazy val classMappingFactory = HashMap[ClassSymbol, (Type, Env) => (Value, Env)](
    ArrayClass -> {(tpe, env) => (new EmulatedArray(tpe), env)}
  )

  def createModule(mod: ModuleSymbol, env: Env): Result = {
    moduleMappingFactory.getOrElse(mod, { (m: ModuleSymbol, e: Env) =>
      val value = new UninitializedModuleValue(m)
      (value, e.extend(m, value))
    })(mod, env)
  }

  def createInstance(tpe: Type, env: Env): Result = {
    classMappingFactory.getOrElse(tpe.typeSymbol.asClass, { (t: Type, e: Env) =>
      val v = new ObjectValue(t.typeSymbol, t)
      (v, e.extend(v, new Object(ListMap())))
    })(tpe, env)
  }

  def ec(f:(List[Value], Env) => Result, nullary: => Boolean, e: Env) =
    (new EmulatedCallableValue(f) { override def isNullary = nullary }, e)

}
