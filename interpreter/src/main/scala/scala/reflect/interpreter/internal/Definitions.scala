package scala.reflect.interpreter
package internal

trait Definitions {
  self: Engine =>

  import u._
  import definitions._

  lazy val Any_isInstanceOf = AnyClass.info.decl(TermName("isInstanceOf"))
  lazy val Any_equals = AnyClass.info.decl(TermName("equals"))
  lazy val Object_eq = ObjectClass.info.decl(TermName("eq"))
  lazy val Option_isDefined = OptionClass.info.decl(TermName("isDefined"))
  lazy val Option_get = OptionClass.info.decl(TermName("get"))
}