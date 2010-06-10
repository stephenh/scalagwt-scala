/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package repl

import scala.collection.mutable.HashSet

trait Imports {
  self: Interpreter =>
  
  import compiler._
  
  /** Compute imports that allow definitions from previous
   *  requests to be visible in a new request.  Returns
   *  three pieces of related code:
   *
   *  1. An initial code fragment that should go before
   *  the code of the new request.
   *
   *  2. A code fragment that should go after the code
   *  of the new request.
   *
   *  3. An access path which can be traverested to access
   *  any bindings inside code wrapped by #1 and #2 .
   *
   * The argument is a set of Names that need to be imported.
   *
   * Limitations: This method is not as precise as it could be.
   * (1) It does not process wildcard imports to see what exactly
   * they import.
   * (2) If it imports any names from a request, it imports all
   * of them, which is not really necessary.
   * (3) It imports multiple same-named implicits, but only the
   * last one imported is actually usable.
   */
  /* private[nsc] */case class ComputedImports(prepend: String, append: String, access: String)
  /* private[nsc] */def importsCode(wanted: Set[Name]): ComputedImports = {
    /** Narrow down the list of requests from which imports 
     *  should be taken.  Removes requests which cannot contribute
     *  useful imports for the specified set of wanted names.
     */
    case class ReqAndHandler(req: Req, handler: MemberHandler) { }
    
    def reqsToUse: List[ReqAndHandler] = {      
      /** Loop through a list of MemberHandlers and select which ones to keep.
        * 'wanted' is the set of names that need to be imported.
       */
      def select(reqs: List[ReqAndHandler], wanted: Set[Name]): List[ReqAndHandler] = {
        // Single symbol imports might be implicits! See bug #1752.  Rather than
        // try to finesse this, we will mimic all imports for now.
        def keepHandler(handler: MemberHandler) = handler match {
          case _: ImportHandler => true
          case x                => x.definesImplicit || (x.declaredNames exists wanted)
        }
                   
        reqs match {
          case Nil                                    => Nil
          case rh :: rest if !keepHandler(rh.handler) => select(rest, wanted)
          case rh :: rest                             =>
            val importedNames = rh.handler match { case x: ImportHandler => x.importedNames ; case _ => Nil }
            import rh.handler._
            val newWanted = wanted ++ referencedNames -- declaredNames -- importedNames
            rh :: select(rest, newWanted)
        }
      }
      
      /** Flatten the handlers out and pair each with the original request */
      select(allReqAndHandlers reverseMap { case (r, h) => ReqAndHandler(r, h) }, wanted).reverse
    }

    val code, trailingBraces, accessPath = new StringBuffer
    val currentImps = HashSet[Name]()

    // add code for a new object to hold some imports
    def addWrapper() {
      code append "object %s {\n".format(nme.INTERPRETER_IMPORT_WRAPPER)
      trailingBraces append "}\n"
      accessPath append ("." + nme.INTERPRETER_IMPORT_WRAPPER)

      currentImps.clear
    }

    addWrapper()

    // loop through previous requests, adding imports for each one
    for (ReqAndHandler(req, handler) <- reqsToUse) {
      handler match {
        // If the user entered an import, then just use it; add an import wrapping
        // level if the import might conflict with some other import
        case x: ImportHandler =>
          val importedSet = x.importedNames.toSet
          
          if (x.importsWildcard || (currentImps exists importedSet))
            addWrapper()
          
          code append (x.member.toString + "\n")
          
          // give wildcard imports a import wrapper all to their own
          if (x.importsWildcard) addWrapper()
          else currentImps ++= importedSet

        // For other requests, import each bound variable.
        // import them explicitly instead of with _, so that
        // ambiguity errors will not be generated. Also, quote 
        // the name of the variable, so that we don't need to 
        // handle quoting keywords separately. 
        case x =>
          for (imv <- x.declaredNames) {
            if (currentImps contains imv) addWrapper()
        
            code append ("import %s\n" format (req pathTo imv))
            currentImps += imv
          }
      }
    }
    // add one extra wrapper, to prevent warnings in the common case of
    // redefining the value bound in the last interpreter request.
    addWrapper()
    ComputedImports(code.toString, trailingBraces.toString, accessPath.toString)
  }
}

