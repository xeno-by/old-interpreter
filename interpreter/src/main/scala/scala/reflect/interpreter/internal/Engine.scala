package scala.reflect.interpreter
package internal

abstract class Engine extends InterpreterRequires with Errors {
  import u._
  import definitions._

  def eval(tree: Tree): Any = {
    // can only interpret fully attributes trees
    // which is why we can't test the interpreter on the output of reify
    // TODO: in Palladium this will become irrelevant, because all trees
    // are going to have a `Tree.tpe` method that will actually typecheck stuff if necessary
    tree.foreach(sub => {
      if (sub.tpe == null) UnattributedAst(sub)
      if (sub.symbol == NoSymbol) UnattributedAst(sub)
    })
    val initialEnv = Env(Scope(), Heap())
    val Result(value, finalEnv) = eval(tree, initialEnv)
    value.reify.getOrElse(UnreifiableResult(value))
  }

  def eval(tree: Tree, env: Env): Result = tree match {
    case EmptyTree                            => eval(q"()", env) // when used in blocks, means "skip that tree", so we evaluate to whatever
    case Literal(_)                           => evalLiteral(tree, env)
    case New(_)                               => ???
    case Ident(_)                             => ??? // q"$_" would've matched any tree, not just an ident
    case q"$qual.$_"                          => ???
    case q"$qual.super[$_].$_"                => ???
    case q"$_.this"                           => ???
    case Apply(expr, args)                    => ??? // the q"$expr[..$targs](...$argss)" quasiquote is too high-level for this
    case TypeApply(expr, targs)               => eval(expr, env)
    case q"$lhs = $rhs"                       => evalAssign(lhs, rhs, env)
    case q"return $expr"                      => ???
    case q"throw $expr"                       => ???
    case q"$expr: $_"                         => eval(expr, env)
    // case q"(..$exprs)"                     => never going to happen, because parser desugars these trees into applications
    case q"{ ..$stats }"                      => evalBlock(stats, env)
    case q"if ($cond) $then1 else $else1"     => ???
    case q"$scrut match { case ..$cases }"    => ???
    case q"try $expr catch { case ..$cases } finally $finally1" => ???
    case q"(..$params) => $body"              => ???
    // case q"{ case ..$cases }"              => never going to happen, because typer desugars these trees into anonymous class instantiations
    case q"while ($cond) $body"               => ???
    case q"do $body while ($cond)"            => ???
    // case q"for (..$enums) $expr"           => never going to happen, because parser desugars these trees into applications
    // case q"for (..$enums) yield $expr"     => never going to happen, because parser desugars these trees into applications
    // case q"new { ..$early } with ..$parents { $self => ..$stats }" => never going to happen in general case, desugared into selects/applications of New
    case _: ValDef | _: ModuleDef | _: DefDef => evalLocal(tree, env) // can't skip these trees, because we need to enter them in scope when interpreting
    case _: MemberDef                         => eval(q"()", env) // skip these trees, because we have sym.source
    case _: Import                            => eval(q"()", env) // skip these trees, because it's irrelevant after typer, which has resolved all imports
    case _                                    => UnrecognizedAst(tree)
  }

  def eval(args: List[Tree], env: Env): Results = {
    args match {
      case arg :: tail =>
        val Result(varg, env1) = eval(arg, env)
        val Results(vtail, env2) = eval(tail, env1)
        Results(varg :: vtail, env2)
      case Nil =>
        Results(Nil, env)
    }
  }

  def evalLiteral(tree: Tree, env: Env): Result = {
    // this tedious pattern match is here to make sure that
    // we only support literal types that we are interested in
    // sure we could say `case Literal(Constant(x)) => reflect(x)`
    // but that would make evaluation less safe
    def reflect(jvmValue: Any) = Value.reflect(jvmValue, env)
    tree match {
      case q"${x: Byte}"                  => reflect(x)
      case q"${x: Short}"                 => reflect(x)
      case q"${x: Char}"                  => reflect(x)
      case q"${x: Int}"                   => reflect(x)
      case q"${x: Long}"                  => reflect(x)
      case q"${x: Float}"                 => reflect(x)
      case q"${x: Double}"                => reflect(x)
      case q"${x: Boolean}"               => reflect(x)
      case q"${x: Unit}"                  => reflect(x)
      case q"${x: String}"                => reflect(x)
      case q"null"                        => reflect(null)
      case Literal(Constant(tpe: Type))   => RuntimeReflectionNotSupported(tree) // this tree shape stands for `classOf[$tpe]` after typechecking
      case Literal(Constant(sym: Symbol)) => UnrecognizedAst(tree) // this is a very obscure tree shape only used in annotations, and we don't eval those
      case _                              => UnrecognizedAst(tree)
    }
  }

  def evalLocal(tree: Tree, env: Env): Result = {
    val sym = tree.symbol
    def defaultValue(tpe: Type): Result = {
      val jvmValue = sym match {
        case sym if sym == UnitClass    => ()
        case sym if sym == BooleanClass => false
        case sym if sym == FloatClass   => 0.0f
        case sym if sym == DoubleClass  => 0.0d
        case sym if sym == ByteClass    => 0.toByte
        case sym if sym == ShortClass   => 0.toShort
        case sym if sym == IntClass     => 0
        case sym if sym == LongClass    => 0L
        case sym if sym == CharClass    => 0.toChar
        case _                          => null
      }
      Value.reflect(jvmValue, env)
    }
    val Result(vrepr, env1) = tree match {
      case ValDef(_, _, tpt, rhs) =>
        // note how we bind ValDef's name to the default value of the corresponding type when evaluating the rhs
        // this is how Scala does it, so don't say that this looks weird :)
        val Result(vdefault, envx) = defaultValue(tpt.tpe)
        eval(rhs, envx.extend(tree.symbol, vdefault))
      case tree: ModuleDef =>
        Value.module(tree.symbol.asModule, env)
      case tree: DefDef =>
        Value.method(tree.symbol.asMethod, env)
    }
    val env2 = env.extend(env1.heap).extend(tree.symbol, vrepr)
    Value.reflect((), env2)
  }

  def evalAssign(lhs: Tree, rhs: Tree, env: Env): Result = {
    // note that we don't need to process assignments that are desugared to `update` calls
    // these end up being processed in evalApply
    lhs match {
      case Ident(_) => // local value (things like `x` as in `this.x` have already been desugared by typer)
        val Result(vrhs, env1) = eval(rhs, env)
        Value.reflect((), env1.extend(lhs.symbol, vrhs))
      case Select(qual, _) => // someone's field
        val Result(vlhs, env1) = eval(lhs, env)
        val Result(vrhs, env2) = eval(rhs, env1)
        Value.reflect((), env1.extend(vlhs, lhs.symbol, vrhs))
    }
  }

  def evalBlock(stats: List[Tree], env: Env): Result = {
    // note how thanks to immutability of envs we don't have problems with local variable scoping
    // when evaluating a block, we can delegate introduction of locals to eval - it will update env and push the updates to us
    // when exiting a block, we just drop the local environment that we have accumulated without having to rollback anything
    val Results(_ :+ vstats, env1) = eval(stats, env)
    Result(vstats, env.extend(env1.heap))
  }

  final case class Scope() // TODO: figure out how to combine both lexical scope (locals and globals) and stack frames
  final case class Heap() // TODO: figure out the API for the heap
  final case class Env(scope: Scope, heap: Heap) {
    def extend(sym: Symbol, value: Value): Env = {
      // TODO: extend scope with a local symbol bound to an associated value
      ???
    }
    def extend(obj: Value, field: Symbol, value: Value): Env = {
      // TODO: extend heap with the new value for the given field of the given object
      ???
    }
    def extend(heap: Heap): Env = {
      // TODO: import heap from another environment
      ???
    }
  }

  sealed trait Value {
    def reify: Option[Any] = {
      // TODO: convert this interpreter value to a JVM value
      // return None if it refers to a not-yet-compiled class
      // note that it is probably possible to improve reify to work correctly in all cases
      // however this doesn't matter much for Project Palladium, so that's really low priority
      ???
    }
  }
  object Value {
    def reflect(any: Any, env: Env): Result = {
      // TODO: wrap a JVM value in an interpreter value
      // strictly speaking, env is unnecessary here, because this shouldn't be effectful
      // but I'm still threading it though here, because who knows
      ???
    }
    def module(mod: ModuleSymbol, env: Env): Result = {
      // TODO: create an interpreter value that corresponds to the object represented by the symbol
      ???
    }
    def method(meth: MethodSymbol, env: Env): Result = {
      // TODO: create an interpreter value that corresponds to the method represented by the symbol
      ???
    }
  }

  final case class Result(value: Value, env: Env)
  final case class Results(value: List[Value], env: Env)
}