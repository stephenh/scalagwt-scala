/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.collection.mutable.ListBuffer
import annotation.tailrec

/**
 *  @author  Martin Odersky
 *  @version 1.0 
 */
trait Contexts { self: Analyzer =>
  import global._

  val NoContext = new Context {
    override def implicitss: List[List[ImplicitInfo]] = List()
    outer = this
    override def toString = "NoContext"
  }
  NoContext.enclClass = NoContext
  NoContext.enclMethod = NoContext

  private val startContext = {
    NoContext.make(
    global.Template(List(), emptyValDef, List()) setSymbol global.NoSymbol setType global.NoType,
    global.definitions.RootClass,
    global.definitions.RootClass.info.decls)
  }

  var lastAccessCheckDetails: String = ""

  /** List of symbols to import from in a root context.  Typically that
   *  is `java.lang`, `scala`, and [[scala.Predef]], in that order.  Exceptions:
   *  
   *  - if option `-Yno-imports` is given, nothing is imported
   *  - if the unit is java defined, only `java.lang` is imported
   *  - if option `-Yno-predef` is given, if the unit has an import of Predef
   *    among its leading imports, or if the unit is [[scala.ScalaObject]]
   *    or [[scala.Predef]], `Predef` is not imported.
   */
  protected def rootImports(unit: CompilationUnit, tree: Tree): List[Symbol] = {
    import definitions._
    assert(isDefinitionsInitialized, "definitions uninitialized")

    if (settings.noimports.value) Nil
    else if (unit.isJava) List(JavaLangPackage)
    else if (settings.nopredef.value || treeInfo.noPredefImportForUnit(unit.body)) List(JavaLangPackage, ScalaPackage)
    else List(JavaLangPackage, ScalaPackage, PredefModule)
  }

  def rootContext(unit: CompilationUnit): Context =
    rootContext(unit, EmptyTree, false)

  def rootContext(unit: CompilationUnit, tree: Tree, erasedTypes: Boolean): Context = {
    import definitions._
    var sc = startContext
    def addImport(pkg: Symbol) {
      assert(pkg ne null)
      val qual = gen.mkAttributedStableRef(pkg)
      sc = sc.makeNewImport(
        Import(qual, List(ImportSelector(nme.WILDCARD, -1, null, -1)))
        .setSymbol(NoSymbol.newImport(NoPosition).setFlag(SYNTHETIC).setInfo(ImportType(qual)))
        .setType(NoType))
      sc.depth += 1
    }
    for (imp <- rootImports(unit, tree))
      addImport(imp)
    val c = sc.make(unit, tree, sc.owner, sc.scope, sc.imports)
    c.reportAmbiguousErrors = !erasedTypes
    c.reportGeneralErrors = !erasedTypes
    c.implicitsEnabled = !erasedTypes
    c
  }

  def resetContexts() {
    var sc = startContext
    while (sc != NoContext) {
      sc.tree match {
        case Import(qual, _) => qual.tpe = singleType(qual.symbol.owner.thisType, qual.symbol)
        case _ =>
      }
      sc = sc.outer
    }
  }

  class Context private[typechecker] {
    var unit: CompilationUnit = NoCompilationUnit
    var tree: Tree = _                      // Tree associated with this context
    var owner: Symbol = NoSymbol            // The current owner
    var scope: Scope = _                    // The current scope
    var outer: Context = _                  // The next outer context
    var enclClass: Context = _              // The next outer context whose tree is a
                                            // template or package definition
    var enclMethod: Context = _             // The next outer context whose tree is a method
    var variance: Int = _                   // Variance relative to enclosing class
    private var _undetparams: List[Symbol] = List() // Undetermined type parameters,
                                                    // not inherited to child contexts
    var depth: Int = 0
    var imports: List[ImportInfo] = List()   // currently visible imports
    var openImplicits: List[(Type,Symbol)] = List()   // types for which implicit arguments
                                             // are currently searched
    // for a named application block (Tree) the corresponding NamedApplyInfo
    var namedApplyBlockInfo: Option[(Tree, NamedApplyInfo)] = None
    var prefix: Type = NoPrefix
    var inConstructorSuffix = false         // are we in a secondary constructor
                                            // after the this constructor call?
    var returnsSeen = false                 // for method context: were returns encountered?
    var inSelfSuperCall = false             // is this context (enclosed in) a constructor call?
    // (the call to the super or self constructor in the first line of a constructor)
    // in this context the object's fields should not be in scope

    var reportAmbiguousErrors = false
    var reportGeneralErrors = false
    var diagnostic: List[String] = Nil      // these messages are printed when issuing an error
    var implicitsEnabled = false
    var checking = false
    var retyping = false

    var savedTypeBounds: List[(Symbol, Type)] = List() // saved type bounds
       // for type parameters which are narrowed in a GADT

    var typingIndentLevel: Int = 0
    def typingIndent = "  " * typingIndentLevel

    def undetparamsString =
      if (undetparams.isEmpty) ""
      else undetparams.mkString("undetparams=", ", ", "")
    def undetparams = _undetparams
    def undetparams_=(ps: List[Symbol]) = { _undetparams = ps }

    def extractUndetparams() = {
      val tparams = undetparams
      undetparams = List()
      tparams
    }

    def withoutReportingErrors[T](op: => T): T = {
      val saved = reportGeneralErrors
      reportGeneralErrors = false
      try op
      finally reportGeneralErrors = saved
    }

    def withImplicitsDisabled[T](op: => T): T = {
      val saved = implicitsEnabled
      implicitsEnabled = false
      try op
      finally implicitsEnabled = saved
    }

    def make(unit: CompilationUnit, tree: Tree, owner: Symbol,
             scope: Scope, imports: List[ImportInfo]): Context = {
      val c   = new Context
      c.unit  = unit
      c.tree  = tree
      c.owner = owner
      c.scope = scope
      c.outer = this

      tree match {
        case Template(_, _, _) | PackageDef(_, _) =>
          c.enclClass = c
          c.prefix = c.owner.thisType
          c.inConstructorSuffix = false
        case _ =>
          c.enclClass = this.enclClass
          c.prefix =
            if (c.owner != this.owner && c.owner.isTerm) NoPrefix
            else this.prefix
          c.inConstructorSuffix = this.inConstructorSuffix
      }
      tree match {
        case DefDef(_, _, _, _, _, _) =>
          c.enclMethod = c
        case _ =>
          c.enclMethod = this.enclMethod
      }
      c.variance = this.variance
      c.depth = if (scope == this.scope) this.depth else this.depth + 1
      c.imports = imports
      c.inSelfSuperCall = inSelfSuperCall
      c.reportAmbiguousErrors = this.reportAmbiguousErrors
      c.reportGeneralErrors = this.reportGeneralErrors
      c.diagnostic = this.diagnostic
      c.typingIndentLevel = typingIndentLevel
      c.implicitsEnabled = this.implicitsEnabled
      c.checking = this.checking
      c.retyping = this.retyping
      c.openImplicits = this.openImplicits
      registerContext(c.asInstanceOf[analyzer.Context])
      debuglog("Created context: " + this + " ==> " + c)
      c
    }

    def make(unit: CompilationUnit): Context = {
      val c = make(unit, EmptyTree, owner, scope, imports)
      c.reportAmbiguousErrors = true
      c.reportGeneralErrors = true
      c.implicitsEnabled = true
      c
    }

    def makeNewImport(imp: Import): Context =
      make(unit, imp, owner, scope, new ImportInfo(imp, depth) :: imports)

    def make(tree: Tree, owner: Symbol, scope: Scope): Context =
      if (tree == this.tree && owner == this.owner && scope == this.scope) this
      else make0(tree, owner, scope)

    private def make0(tree: Tree, owner: Symbol, scope: Scope): Context =
      make(unit, tree, owner, scope, imports)

    def makeNewScope(tree: Tree, owner: Symbol): Context =
      make(tree, owner, new Scope(scope))
    // IDE stuff: distinguish between scopes created for typing and scopes created for naming. 

    def make(tree: Tree, owner: Symbol): Context =
      make0(tree, owner, scope)

    def make(tree: Tree): Context =
      make(tree, owner)

    def makeSilent(reportAmbiguousErrors: Boolean, newtree: Tree = tree): Context = {
      val c = make(newtree)
      c.reportGeneralErrors = false
      c.reportAmbiguousErrors = reportAmbiguousErrors
      c
    }

    def makeImplicit(reportAmbiguousErrors: Boolean) = {
      val c = makeSilent(reportAmbiguousErrors)
      c.implicitsEnabled = false
      c
    }

    def makeConstructorContext = {
      var baseContext = enclClass.outer
      //todo: find out why we need next line
      while (baseContext.tree.isInstanceOf[Template])
        baseContext = baseContext.outer
      val argContext = baseContext.makeNewScope(tree, owner)
      argContext.inSelfSuperCall = true
      argContext.reportGeneralErrors = this.reportGeneralErrors
      argContext.reportAmbiguousErrors = this.reportAmbiguousErrors
      def enterElems(c: Context) {
        def enterLocalElems(e: ScopeEntry) {
          if (e != null && e.owner == c.scope) {
            enterLocalElems(e.next)
            argContext.scope enter e.sym
          }
        }
        if (c.owner.isTerm && !c.owner.isLocalDummy) {
          enterElems(c.outer)
          enterLocalElems(c.scope.elems)
        }
      }
      enterElems(this)
      argContext
    }

    private def addDiagString(msg: String) = {
      val ds =
        if (diagnostic.isEmpty) ""
        else diagnostic.mkString("\n","\n", "")
      if (msg endsWith ds) msg else msg + ds
    }

    private def unitError(pos: Position, msg: String) = 
      unit.error(pos, if (checking) "\n**** ERROR DURING INTERNAL CHECKING ****\n" + msg else msg)

    def error(pos: Position, err: Throwable) =
      if (reportGeneralErrors) unitError(pos, addDiagString(err.getMessage()))
      else throw err

    def error(pos: Position, msg: String) = {
      val msg1 = addDiagString(msg)
      if (reportGeneralErrors) unitError(pos, msg1)
      else throw new TypeError(pos, msg1)
    }

/*!!!    def errorTree(pos: Position, msg: String): ErrorTree = {
      error(pos, msg)
      ErrorTree(msg) setPos pos
    }
    */

    def warning(pos:  Position, msg: String) = {
      if (reportGeneralErrors) unit.warning(pos, msg)
    }

    def ambiguousError(pos: Position, pre: Type, sym1: Symbol, sym2: Symbol, rest: String) {
      val (reportPos, msg) = (
        if (sym1.hasDefaultFlag && sym2.hasDefaultFlag && sym1.enclClass == sym2.enclClass) {
          val methodName = nme.defaultGetterToMethod(sym1.name)
          (sym1.enclClass.pos,            
           "in "+ sym1.enclClass +", multiple overloaded alternatives of " + methodName +
                     " define default arguments")
        }
        else {
          (pos,
            ("ambiguous reference to overloaded definition,\n" +
             "both " + sym1 + sym1.locationString + " of type " + pre.memberType(sym1) +
             "\nand  " + sym2 + sym2.locationString + " of type " + pre.memberType(sym2) +
             "\nmatch " + rest)
          )
        }
      )
      if (reportAmbiguousErrors) {
        if (!pre.isErroneous && !sym1.isErroneous && !sym2.isErroneous)
          unit.error(reportPos, msg)
      } else throw new TypeError(pos, msg)
    }

    def isLocal(): Boolean = tree match {
      case Block(_,_)       => true
      case PackageDef(_, _) => false
      case EmptyTree        => false
      case _                => outer.isLocal()
    }

    // nextOuter determines which context is searched next for implicits
    // (after `this`, which contributes `newImplicits` below.) In
    // most cases, it is simply the outer context: if we're owned by
    // a constructor, the actual current context and the conceptual
    // context are different when it comes to scoping. The current
    // conceptual scope is the context enclosing the blocks which
    // represent the constructor body (TODO: why is there more than one
    // such block in the outer chain?)
    private def nextOuter = {
      // Drop the constructor body blocks, which come in varying numbers.
      // -- If the first statement is in the constructor, scopingCtx == (constructor definition)
      // -- Otherwise, scopingCtx == (the class which contains the constructor)
      val scopingCtx =
        if (owner.isConstructor) nextEnclosing(c => !c.tree.isInstanceOf[Block])
        else this

      scopingCtx.outer
    }

    def nextEnclosing(p: Context => Boolean): Context =
      if (this == NoContext || p(this)) this else outer.nextEnclosing(p)

    override def toString = "Context(%s@%s unit=%s scope=%s)".format(
      owner.fullName, tree.shortClass, unit, scope.##
    )
    /** Is `sub` a subclass of `base` or a companion object of such a subclass?
     */
    def isSubClassOrCompanion(sub: Symbol, base: Symbol) = 
      sub.isNonBottomSubClass(base) ||
      sub.isModuleClass && sub.linkedClassOfClass.isNonBottomSubClass(base)

    /** Return closest enclosing context that defines a superclass of `clazz`, or a
     *  companion module of a superclass of `clazz`, or NoContext if none exists */
    def enclosingSuperClassContext(clazz: Symbol): Context = {
      var c = this.enclClass
      while (c != NoContext && 
             !clazz.isNonBottomSubClass(c.owner) &&
             !(c.owner.isModuleClass && clazz.isNonBottomSubClass(c.owner.companionClass)))
        c = c.outer.enclClass
      c
    }

    /** Return the closest enclosing context that defines a subclass of `clazz`
     *  or a companion object thereof, or `NoContext` if no such context exists.
     */
    def enclosingSubClassContext(clazz: Symbol): Context = {
      var c = this.enclClass
      while (c != NoContext && !isSubClassOrCompanion(c.owner, clazz))
        c = c.outer.enclClass
      c
    }

    /** Is `sym` accessible as a member of tree `site` with type
     *  `pre` in current context?
     */
    def isAccessible(sym: Symbol, pre: Type, superAccess: Boolean = false): Boolean = {
      lastAccessCheckDetails = ""
      // Console.println("isAccessible(%s, %s, %s)".format(sym, pre, superAccess))

      @inline def accessWithinLinked(ab: Symbol) = {
        val linked = ab.linkedClassOfClass
        // don't have access if there is no linked class
        // (before adding the `ne NoSymbol` check, this was a no-op when linked eq NoSymbol,
        //  since `accessWithin(NoSymbol) == true` whatever the symbol)
        (linked ne NoSymbol) && accessWithin(linked)
      }

      /** Are we inside definition of `ab`? */
      def accessWithin(ab: Symbol) = {
        // #3663: we must disregard package nesting if sym isJavaDefined
        if (sym.isJavaDefined) {
          // is `o` or one of its transitive owners equal to `ab`?
          // stops at first package, since further owners can only be surrounding packages
          @tailrec def abEnclosesStopAtPkg(o: Symbol): Boolean =
            (o eq ab) || (!o.isPackageClass && (o ne NoSymbol) && abEnclosesStopAtPkg(o.owner))
          abEnclosesStopAtPkg(owner)
        } else (owner hasTransOwner ab)
      }

/*
        var c = this
        while (c != NoContext && c.owner != owner) {
          if (c.outer eq null) assert(false, "accessWithin(" + owner + ") " + c);//debug
          if (c.outer.enclClass eq null) assert(false, "accessWithin(" + owner + ") " + c);//debug
          c = c.outer.enclClass
        }
        c != NoContext
      }
*/
      /** Is `clazz` a subclass of an enclosing class? */
      def isSubClassOfEnclosing(clazz: Symbol): Boolean =
        enclosingSuperClassContext(clazz) != NoContext

      def isSubThisType(pre: Type, clazz: Symbol): Boolean = pre match {
        case ThisType(pclazz) => pclazz isNonBottomSubClass clazz
        case _ => false
      }

      /** Is protected access to target symbol permitted */
      def isProtectedAccessOK(target: Symbol) = {
        val c = enclosingSubClassContext(sym.owner)
        if (c == NoContext)
          lastAccessCheckDetails =
            "\n Access to protected "+target+" not permitted because"+
            "\n "+"enclosing "+this.enclClass.owner+
            this.enclClass.owner.locationString+" is not a subclass of "+
            "\n "+sym.owner+sym.owner.locationString+" where target is defined"
        c != NoContext &&
        {
          target.isType || { // allow accesses to types from arbitrary subclasses fixes #4737
            val res =
              isSubClassOrCompanion(pre.widen.typeSymbol, c.owner) ||
              c.owner.isModuleClass &&
              isSubClassOrCompanion(pre.widen.typeSymbol, c.owner.linkedClassOfClass)
            if (!res)
              lastAccessCheckDetails =
                "\n Access to protected "+target+" not permitted because"+
                "\n prefix type "+pre.widen+" does not conform to"+
                "\n "+c.owner+c.owner.locationString+" where the access take place"
              res
          }
        }
      }

      (pre == NoPrefix) || {
        val ab = sym.accessBoundary(sym.owner)

        (  (ab.isTerm || ab == definitions.RootClass)
        || (accessWithin(ab) || accessWithinLinked(ab)) &&
             (  !sym.hasLocalFlag
             || sym.owner.isImplClass // allow private local accesses to impl classes
             || sym.isProtected && isSubThisType(pre, sym.owner)
             || pre =:= sym.owner.thisType
             )
        || sym.isProtected &&
             (  superAccess
             || pre.isInstanceOf[ThisType]
             || phase.erasedTypes 
             || isProtectedAccessOK(sym)
             || (sym.allOverriddenSymbols exists isProtectedAccessOK)
                // that last condition makes protected access via self types work.
             )
        )
        // note: phase.erasedTypes disables last test, because after addinterfaces
        // implementation classes are not in the superclass chain. If we enable the
        // test, bug780 fails.
      }
    }

    def pushTypeBounds(sym: Symbol) {
      savedTypeBounds ::= ((sym, sym.info))
    }

    def restoreTypeBounds(tp: Type): Type = {
      var current = tp
      for ((sym, info) <- savedTypeBounds) {
        debuglog("resetting " + sym + " to " + info);
        sym.info match {
          case TypeBounds(lo, hi) if (hi <:< lo && lo <:< hi) =>
            current = current.instantiateTypeParams(List(sym), List(lo))
//@M TODO: when higher-kinded types are inferred, probably need a case PolyType(_, TypeBounds(...)) if ... =>            
          case _ =>
        }
        sym.setInfo(info)
      }
      savedTypeBounds = List()
      current
    }

    private var implicitsCache: List[List[ImplicitInfo]] = null
    private var implicitsRunId = NoRunId

    def resetCache() {
      implicitsRunId = NoRunId
      implicitsCache = null
      if (outer != null && outer != this) outer.resetCache()
    }

    /** A symbol `sym` qualifies as an implicit if it has the IMPLICIT flag set,
     *  it is accessible, and if it is imported there is not already a local symbol
     *  with the same names. Local symbols override imported ones. This fixes #2866.
     */
    private def isQualifyingImplicit(sym: Symbol, pre: Type, imported: Boolean) =
      sym.isImplicit &&
      isAccessible(sym, pre) && 
      !(imported && {
        val e = scope.lookupEntry(sym.name)
        (e ne null) && (e.owner == scope)
      })

    private def collectImplicits(syms: List[Symbol], pre: Type, imported: Boolean = false): List[ImplicitInfo] =
      for (sym <- syms if isQualifyingImplicit(sym, pre, imported)) yield
        new ImplicitInfo(sym.name, pre, sym)

    private def collectImplicitImports(imp: ImportInfo): List[ImplicitInfo] = {
      val pre = imp.qual.tpe
      def collect(sels: List[ImportSelector]): List[ImplicitInfo] = sels match {
        case List() => 
          List()
        case List(ImportSelector(nme.WILDCARD, _, _, _)) => 
          collectImplicits(pre.implicitMembers, pre, imported = true)
        case ImportSelector(from, _, to, _) :: sels1 => 
          var impls = collect(sels1) filter (info => info.name != from)
          if (to != nme.WILDCARD) {
            for (sym <- imp.importedSymbol(to).alternatives)
              if (isQualifyingImplicit(sym, pre, imported = true))
                impls = new ImplicitInfo(to, pre, sym) :: impls
          }
          impls
      }
      //debuglog("collect implicit imports " + imp + "=" + collect(imp.tree.selectors))//DEBUG
      collect(imp.tree.selectors)
    }

    def implicitss: List[List[ImplicitInfo]] = {
      if (implicitsRunId != currentRunId) {
        implicitsRunId = currentRunId
        implicitsCache = List()
        val newImplicits: List[ImplicitInfo] =
          if (owner != nextOuter.owner && owner.isClass && !owner.isPackageClass && !inSelfSuperCall) {
            if (!owner.isInitialized) return nextOuter.implicitss
            // debuglog("collect member implicits " + owner + ", implicit members = " + owner.thisType.implicitMembers)//DEBUG
            val savedEnclClass = enclClass
            this.enclClass = this
            val res = collectImplicits(owner.thisType.implicitMembers, owner.thisType)
            this.enclClass = savedEnclClass
            res
          } else if (scope != nextOuter.scope && !owner.isPackageClass) {
            debuglog("collect local implicits " + scope.toList)//DEBUG
            collectImplicits(scope.toList, NoPrefix)
          } else if (imports != nextOuter.imports) {
            assert(imports.tail == nextOuter.imports)
            collectImplicitImports(imports.head)
          } else if (owner.isPackageClass) { 
            // the corresponding package object may contain implicit members.
            collectImplicits(owner.tpe.implicitMembers, owner.tpe)
          } else List()
        implicitsCache = if (newImplicits.isEmpty) nextOuter.implicitss
                         else newImplicits :: nextOuter.implicitss
      }
      implicitsCache
    }

    /**
     * Find a symbol in this context or one of its outers.
     *
     * Used to find symbols are owned by methods (or fields), they can't be
     * found in some scope.
     *
     * Examples: companion module of classes owned by a method, default getter
     * methods of nested methods. See NamesDefaults.scala
     */
    def lookup(name: Name, expectedOwner: Symbol) = {
      var res: Symbol = NoSymbol
      var ctx = this
      while (res == NoSymbol && ctx.outer != ctx) {
        val s = ctx.scope lookup name
        if (s != NoSymbol && s.owner == expectedOwner)
          res = s
        else
          ctx = ctx.outer
      }
      res
    }
  } //class Context

  class ImportInfo(val tree: Import, val depth: Int) {
    /** The prefix expression */
    def qual: Tree = tree.symbol.info match {
      case ImportType(expr) => expr
      case ErrorType => tree setType NoType // fix for #2870
      case _ => throw new FatalError("symbol " + tree.symbol + " has bad type: " + tree.symbol.info) //debug
    }

    /** Is name imported explicitly, not via wildcard? */
    def isExplicitImport(name: Name): Boolean =
      tree.selectors exists (_.rename == name.toTermName)

    /** The symbol with name `name` imported from import clause `tree`.
     */
    def importedSymbol(name: Name): Symbol = {
      var result: Symbol = NoSymbol
      var renamed = false
      var selectors = tree.selectors
      while (selectors != Nil && result == NoSymbol) {
        if (selectors.head.rename == name.toTermName)
          result = qual.tpe.nonLocalMember( // new to address #2733: consider only non-local members for imports
            if (name.isTypeName) selectors.head.name.toTypeName else selectors.head.name)
        else if (selectors.head.name == name.toTermName)
          renamed = true
        else if (selectors.head.name == nme.WILDCARD && !renamed)
          result = qual.tpe.nonLocalMember(name)
        selectors = selectors.tail
      }
      result
    }

    def allImportedSymbols: List[Symbol] = 
      qual.tpe.members flatMap (transformImport(tree.selectors, _))

    private def transformImport(selectors: List[ImportSelector], sym: Symbol): List[Symbol] = selectors match {
      case List() => List()
      case List(ImportSelector(nme.WILDCARD, _, _, _)) => List(sym)
      case ImportSelector(from, _, to, _) :: _ if from == sym.name => 
        if (to == nme.WILDCARD) List()
        else { val sym1 = sym.cloneSymbol; sym1.name = to; List(sym1) }
      case _ :: rest => transformImport(rest, sym)
    }

    override def toString() = tree.toString()
  }

  case class ImportType(expr: Tree) extends Type {
    override def safeToString = "ImportType("+expr+")"
  }
}
