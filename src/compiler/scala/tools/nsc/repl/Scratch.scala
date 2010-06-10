/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package repl

trait Scratch {
  
  
  // 
  // def allImplicits = flatCollect(allHandlers) {
  //   case x: MemberHandler if x.definesImplicit => x.declaredNames
  // }

  // /** Another entry point for tab-completion, ids in scope */
  // def awareMap() = termNames() map (x => x -> completionAware(x)) collect { case (x, Some(y)) => x -> y } toMap
  // 
  // def typesOfTerms() = termNames() flatMap (x => stringToTypeOpt(x.toString))
  // 
  // /** Artificial object demonstrating completion */
  // lazy val replDemo = CompletionAware(
  //   Map[String, CompletionAware](
  //     "ids" -> CompletionAware(() => termNames, completionAware _),
  //     "types" -> CompletionAware(() => typesOfTerms),
  //     "implicits" -> CompletionAware(() => allImplicits)
  //   )
  // )
  
      // LineArg("meta", "given code which produces scala code, executes the results",
      //   (xs: List[String]) => )

  
  /** Executes code looking for a manifest of type T.
   */
  // def manifestFor[T: Manifest] =
  //   evalExpr[Manifest[T]]("""manifest[%s]""".format(manifest[T]))

  /** Executes code looking for an implicit value of type T.
   */
  // def implicitFor[T: Manifest] = {
  //   val s = manifest[T].toString
  //   evalExpr[Option[T]]("{ def f(implicit x: %s = null): %s = x ; Option(f) }".format(s, s))
  //   // We don't use implicitly so as to fail without failing.
  //   // evalExpr[T]("""implicitly[%s]""".format(manifest[T]))
  // }
  /** Executes code looking for an implicit conversion from the type
   *  of the given identifier to CompletionAware.
   */
  // def completionAwareImplicit[T](id: String) = {
  //   val f1string = "%s => %s".format(stringToType(id), classOf[CompletionAware].getName)
  //   val code = """{
  //     |  def f(implicit x: (%s) = null): %s = x
  //     |  val f1 = f
  //     |  if (f1 == null) None else Some(f1(%s))
  //     |}""".stripMargin.format(f1string, f1string, id)
  //   
  //   evalExpr[Option[CompletionAware]](code)
  // }
  // 
  
  // interpret("import repl.createUpdater")

  /** Artificial object demonstrating completion */
  // lazy val replVars = CompletionAware(
  //   Map[String, CompletionAware](
  //     "ids" -> CompletionAware(() => termNames, completionAware _),
  //     "synthVars" -> CompletionAware(() => alldeclaredNames filter isSynthVarName map (_.toString)),
  //     "types" -> CompletionAware(() => allSeenTypes map (_.toString)),
  //     "implicits" -> CompletionAware(() => allImplicits map (_.toString))
  //   )
  // )
  
  
  // Coming soon  
  // implicit def string2liftedcode(s: String): LiftedCode = new LiftedCode(s)
  // case class LiftedCode(code: String) {
  //   val lifted: String = {
  //     quietly { interpret(code) }
  //     eval2[String]("({ " + code + " }).toString")
  //   }
  //   def >> : String = lifted
  // }

  
  
  // def power() {
    // val powerUserBanner =
    //   """** Power User mode enabled - BEEP BOOP      **
    //     |** scala.tools.nsc._ has been imported      **
    //     |** New vals! Try repl, global, power        **
    //     |** New cmds! :help to discover them         **
    //     |** New defs! Type power.<tab> to reveal     **""".stripMargin
    // 
    // powerUserOn = true
    // interpreter.unleash()    
    // inject("history", in.historyList)
    // in.completion foreach (x => inject("completion", x))

    // out println powerUserBanner
  // }

  // 
  // abstract class ReplTyped[T: Manifest] {
  //   def path: String
  //   def apply(name: String) = "%s.%s(%s)".format(path, "unapply", name)
  //   def unapply(name: String) =
  //     try stringToResult(name) map (_.asInstanceOf[T])
  //     catch { case _: ClassCastException => None }
  // }
  // object CompletionAwareTyped extends ReplTyped[CompletionAware] {
  //   val path = "repl.CompletionAwareTyped"
  // }

  // XXX literals.
  // if it's a recognized identifier, the type of that; otherwise treat the
  // String like a value (e.g. scala.collection.Map) .  
  // def stringToTree(id: String)    = nameOrElse(id, treeOf, parse(id) collect { case List(x) => x } getOrElse EmptyTree)
  // def stringToSymbol(id: String)  = nameOrElse(id, symOf, moduleOr(id, x => x) getOrElse NoSymbol)
  // def stringToType(id: String)    = nameOrElse(id, tpeOf, typeType(id) getOrElse NoType)
  // def stringToType(id: String)    = nameOrElse(id, tpeOf, moduleOr(id, _.tpe) getOrElse NoType)
  
}