package scala.reflect.interpreter
package internal

trait Emulators {
	this: Engine with InterpreterRequires =>

	import u._ 

	trait PrimitivesEmulator {		
		def selectCallable(value: Value, sym: Symbol): CallableValue
	}

	trait RefletionEmulator extends PrimitivesEmulator {
		def selectCallable(value: Value, sym: Symbol): CallableValue = ???
	}

	trait ExplicitEmulator extends PrimitivesEmulator {
		def selectCallable(value: Value, sym: Symbol): CallableValue = {
			value.reify match {
				case Some(v: Int) => 
					sym.asMethod.name.encodedName match {
						case "$plus" =>
							new CallableValue { 
								override def apply(args: List[Value], env: Env): Result = {
									//we need another match here, this time against arguments
									Result(JvmValue(v + args.head.reify.get.asInstanceOf[Int]), env)
								}
							}
						case "$minus" =>
							new CallableValue { 
								override def apply(args: List[Value], env: Env): Result = {
									Result(JvmValue(v - args.head.reify.get.asInstanceOf[Int]), env)
							}
						}
						case other => println(other); ???					
				}
				case None => ???
			}
		}
	}
}

