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
          case Any_isInstanceOf => binOp[Any, Type](_.getClass == _.getClass)
          case Any_hashCode     => unaryOp[Any](_.hashCode())
          case Object_hashcode  => unaryOp[java.lang.Object](java.util.Objects.hashCode(_))
          case Object_init      => dummyOp
          case Throwable_init   => dummyOp
          case other            => UnsupportedEmulation(sym)
        }
      }){ override def isZeroArg: Boolean = sym.asMethod.paramLists.isEmpty }
    }
  }

  class __Option(mod :ModuleSymbol) extends TypedValue(mod.typeSignature)
  class __None(mod: ModuleSymbol) extends __Option(mod)
  class __SomeModule(mod: ModuleSymbol) extends __Option(mod) {
    class __Some(tpe: Type, val value: Value, env: Env) extends TypedValue(tpe) {
      override def select(member: Symbol, env: Env, static: Boolean): (Value, Env) = {
        if (member.name.toString == "get") (new EmulatedCallableValue({
          (args: List[Value], e: Env) => (value, e)
        }){ override def isZeroArg = true }, env)
        else ???
      }
    }
    override def select(member: Symbol, env: Env, static: Boolean): (Value, Env) = {
      if (member.name.decodedName.toString == "apply") (new EmulatedCallableValue(
        (args: List[Value], env: Env) => { (new __Some(mod.typeSignature, args.head, env), env) }){
        override def isZeroArg = false
      },
      env)
      else (selectCallable(this, member, env), env)
    }
  }

  class __Array(tpe: Type) extends TypedValue(tpe) {
    var data: Array[Value] = null
    def constructFrom(v: Array[Value]): __Array = {data = v; this}
    def constructFrom(t: Type, num: Int, e: Env): Result = {
      val tmp = new mutable.ListBuffer[Result]
      (1 to num).foldLeft(e)((res, _) => (tmp += defaultValue(t, res)).head._2)
      data = tmp.map(_._1).toArray
      (this, tmp.last._2)
    }
    override def select(member: Symbol, env: Env, static: Boolean): (Value, Env) = {
      if (member.isConstructor) {
        (new EmulatedCallableValue((args: List[Value], env: Env) =>
          constructFrom(tpe.typeArgs.head, args.head.reify(env)._1.asInstanceOf[Int], env)) {
          override def isZeroArg = false
        }, env)
      } else if (member.name.toString == "apply") {
        (new EmulatedCallableValue(
          (args: List[Value], env: Env) => (data(args.head.reify(env)._1.asInstanceOf[Int]), env)
        ) { override def isZeroArg = false }, env)
      } else if (member.name.toString == "update") {
        (new EmulatedCallableValue(
          (args: List[Value], env: Env) => {
            data(args.head.reify(env)._1.asInstanceOf[Int]) = args.tail.head
            Value.reflect((), env)
          }
        ) { override def isZeroArg = false }, env)
      } else ???
    }
  }

  class __ArrayModule(mod: ModuleSymbol) extends TypedValue(mod.typeSignature) {
    override def select(member: Symbol, env: Env, static: Boolean): (Value, Env) = {
      if (member.name.decodedName.toString == "apply") (new EmulatedCallableValue(
        (args: List[Value], env: Env) => (new __Array(mod.typeSignature).constructFrom(args.toArray), env)
      ) {  override def isZeroArg = false }, env)
      else ???
    }
  }

  private lazy val moduleMappingFactory = HashMap[ModuleSymbol, (ModuleSymbol, Env) => (Value, Env)](
    NoneModule -> {(symbol, env) => (new __None(symbol), env)},
    SomeModule -> {(symbol, env) => (new __SomeModule(symbol), env)},
    ArrayModule-> {(symbol, env) => (new __ArrayModule(symbol), env)}
  )

  private lazy val classMappingFactory = HashMap[ClassSymbol, (Type, Env) => (Value, Env)](
    ArrayClass -> {(tpe, env) => (new __Array(tpe), env)}
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

}
