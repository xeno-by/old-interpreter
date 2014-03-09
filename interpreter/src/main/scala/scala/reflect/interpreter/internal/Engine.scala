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
    case EmptyTree                            => ???
    case Literal(_)                           => ???
    case New(_)                               => ???
    case Ident(_)                             => ??? // q"$_" would've matched any tree, not just an ident
    case q"$qual.$_"                          => ???
    case q"$qual.super[$_].$_"                => ???
    case q"$_.this"                           => ???
    case Apply(expr, args)                    => ??? // the q"$expr[..$targs](...$argss)" quasiquote is too high-level for this
    case TypeApply(expr, targs)               => ???
    case q"$lhs = $rhs"                       => ???
    case q"return $expr"                      => ???
    case q"throw $expr"                       => ???
    case q"$expr: $_"                         => ???
    // case q"(..$exprs)"                     => never going to happen, because parser desugars these trees into applications
    case q"{ ..$stats }"                      => ???
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
    case _: ValDef | _: ModuleDef | _: DefDef => ???
    case _: MemberDef                         => ???
    case _: Import                            => ???
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

  final case class Scope() // TODO: figure out how to combine both lexical scope (locals and globals) and stack frames
  final case class Heap() // TODO: figure out the API for the heap
  final case class Env(scope: Scope, heap: Heap)
  sealed trait Value {
    def reify: Option[Any] = {
      // TODO: convert this interpreter value to a JVM value
      // return None if it refers to a not-yet-compiled class
      // note that it is probably possible to improve reify to work correctly in all cases
      // however this doesn't matter much for Project Palladium, so that's really low priority
      ???
    }
  }
  final case class Result(value: Value, env: Env)
  final case class Results(value: List[Value], env: Env)
}