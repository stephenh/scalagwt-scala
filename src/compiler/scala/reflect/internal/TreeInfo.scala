/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import Flags._

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class TreeInfo {
  val global: SymbolTable

  import global._ 
  import definitions.{ isVarArgsList, ThrowableClass }

  /* Does not seem to be used. Not sure what it does anyway.
  def isOwnerDefinition(tree: Tree): Boolean = tree match {
    case PackageDef(_, _)
       | ClassDef(_, _, _, _)
       | ModuleDef(_, _, _)
       | DefDef(_, _, _, _, _, _)
       | Import(_, _) => true
    case _ => false
  }
*/
  
  // def isDefinition(tree: Tree): Boolean = tree.isDef

  /** Is tree a declaration or type definition? 
   */
  def isDeclarationOrTypeDef(tree: Tree): Boolean = tree match {
    case DefDef(_, _, _, _, _, EmptyTree)
       | ValDef(_, _, _, EmptyTree)
       | TypeDef(_, _, _, _) => true
    case _ => false
  }

  /** Is tree legal as a member definition of an interface?
   */
  def isInterfaceMember(tree: Tree): Boolean = tree match {
    case EmptyTree                     => true
    case Import(_, _)                  => true
    case TypeDef(_, _, _, _)           => true
    case DefDef(mods, _, _, _, _, __)  => mods.isDeferred
    case ValDef(mods, _, _, _)         => mods.isDeferred
    case _ => false
  }

  /** Is tree a pure (i.e. non-side-effecting) definition?
   */
  def isPureDef(tree: Tree): Boolean = tree match {
    case EmptyTree
       | ClassDef(_, _, _, _)
       | TypeDef(_, _, _, _)
       | Import(_, _)
       | DefDef(_, _, _, _, _, _) =>
      true
    case ValDef(mods, _, _, rhs) =>
      !mods.isMutable && isPureExpr(rhs)
    case _ =>
      false
  }

  /** Is tree a stable and pure expression?
   *  !!! Clarification on what is meant by "pure" here would be appreciated.
   *  This implementation allows both modules and lazy vals, which are pure in
   *  the sense that they always return the same result, but which are also
   *  side effecting.  So for now, "pure" != "not side effecting".
   */
  def isPureExpr(tree: Tree): Boolean = tree match {
    case EmptyTree
       | This(_)
       | Super(_, _)
       | Literal(_) =>
      true
    case Ident(_) =>
      tree.symbol.isStable
    // this case is mostly to allow expressions like -5 and +7, but any
    // member of an anyval should be safely pure
    case Select(Literal(const), name) =>
      const.isAnyVal && (const.tpe.member(name) != NoSymbol)
    case Select(qual, _) =>
      tree.symbol.isStable && isPureExpr(qual)
    case TypeApply(fn, _) =>
      isPureExpr(fn)
    case Apply(fn, List()) =>
      /* Note: After uncurry, field accesses are represented as Apply(getter, Nil),
       * so an Apply can also be pure.
       * However, before typing, applications of nullary functional values are also
       * Apply(function, Nil) trees. To prevent them from being treated as pure,
       * we check that the callee is a method. */
      fn.symbol.isMethod && !fn.symbol.isLazy && isPureExpr(fn)
    case Typed(expr, _) =>
      isPureExpr(expr)
    case Block(stats, expr) =>
      (stats forall isPureDef) && isPureExpr(expr)
    case _ =>
      false
  }


  /**
   * Selects the correct parameter list when there are nested applications.
   * Given Apply(fn, args), args might correspond to any of fn.symbol's parameter
   * lists.  To choose the correct one before uncurry, we have to unwrap any
   * applies: for instance Apply(fn @ Apply(Apply(_, _), _), args) implies args
   * correspond to the third parameter list.
   *
   * Also accounts for varargs.
   */
  def zipMethodParamsAndArgs(t: Tree): List[(Symbol, Tree)] = t match {
    case Apply(fn, args) =>
      val params = fn.symbol.paramss(applyDepth(fn))
      val plen   = params.length
      val alen   = args.length
      def fail() = {
        global.debugwarn(
          "Mismatch trying to zip method parameters and argument list:\n" +
          "  params = " + params + "\n" +
          "    args = " + args + "\n" +
          "    tree = " + t
        )
        params zip args
      }

      if (plen == alen) params zip args
      else if (params.isEmpty) fail
      else if (isVarArgsList(params)) {
        val plenInit = plen - 1
        if (alen == plenInit) {
          if (alen == 0) Nil        // avoid calling mismatched zip
          else params.init zip args
        }
        else if (alen < plenInit) fail
        else {
          val front = params.init zip (args take plenInit)
          val back  = args drop plenInit map (a => (params.last, a))
          front ++ back
        }
      }
      else fail
    case _  => Nil
  }

  /** Is symbol potentially a getter of a variable?
   */
  def mayBeVarGetter(sym: Symbol): Boolean = sym.info match {
    case NullaryMethodType(_)              => sym.owner.isClass && !sym.isStable
    case PolyType(_, NullaryMethodType(_)) => sym.owner.isClass && !sym.isStable
    case mt @ MethodType(_, _)             => mt.isImplicit && sym.owner.isClass && !sym.isStable
    case _                                 => false
  }

  /** Is tree a mutable variable, or the getter of a mutable field?
   */
  def isVariableOrGetter(tree: Tree) = {
    def sym       = tree.symbol
    def isVar     = sym.isVariable
    def isGetter  = mayBeVarGetter(sym) && sym.owner.info.member(nme.getterToSetter(sym.name)) != NoSymbol
    
    tree match {
      case Ident(_)         => isVar
      case Select(_, _)     => isVar || isGetter
      case _                =>
        methPart(tree) match {
          case Select(qual, nme.apply)  => qual.tpe.member(nme.update) != NoSymbol
          case _                        => false
        }
    }
  }

  /** Is tree a self constructor call this(...)? I.e. a call to a constructor of the
   *  same object?
   */
  def isSelfConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Ident(nme.CONSTRUCTOR) 
       | Select(This(_), nme.CONSTRUCTOR) => true
    case _ => false
  }

  /** Is tree a super constructor call?
   */
  def isSuperConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Select(Super(_, _), nme.CONSTRUCTOR) => true
    case _ => false
  }

  /** Is tree a self or super constructor call? */
  def isSelfOrSuperConstrCall(tree: Tree) =
    isSelfConstrCall(tree) || isSuperConstrCall(tree)

  /** Is tree a variable pattern? */
  def isVarPattern(pat: Tree): Boolean = pat match {
    case _: BackQuotedIdent => false
    case x: Ident           => isVariableName(x.name)
    case _                  => false
  }

  /** The first constructor definitions in `stats` */
  def firstConstructor(stats: List[Tree]): Tree = stats find {
    case x: DefDef  => nme.isConstructorName(x.name)
    case _          => false
  } getOrElse EmptyTree
  
  /** The arguments to the first constructor in `stats`. */
  def firstConstructorArgs(stats: List[Tree]): List[Tree] = firstConstructor(stats) match {
    case DefDef(_, _, _, args :: _, _, _) => args
    case _                                => Nil
  }

  /** The value definitions marked PRESUPER in this statement sequence */
  def preSuperFields(stats: List[Tree]): List[ValDef] = 
    stats collect { case vd: ValDef if isEarlyValDef(vd) => vd }

  def isEarlyDef(tree: Tree) = tree match {
    case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
    case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  def isEarlyValDef(tree: Tree) = tree match {
    case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  def isEarlyTypeDef(tree: Tree) = tree match {
    case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  /** Is tpt a vararg type of the form T* ? */
  def isRepeatedParamType(tpt: Tree) = tpt match {
    case TypeTree()                                                          => definitions.isRepeatedParamType(tpt.tpe)
    case AppliedTypeTree(Select(_, tpnme.REPEATED_PARAM_CLASS_NAME), _)      => true
    case AppliedTypeTree(Select(_, tpnme.JAVA_REPEATED_PARAM_CLASS_NAME), _) => true
    case _                                                                   => false
  }
  
  /** The parameter ValDefs of a method definition that have vararg types of the form T* 
   */
  def repeatedParams(tree: Tree): List[ValDef] = tree match {
    case DefDef(_, _, _, vparamss, _, _)  => vparamss.flatten filter (vd => isRepeatedParamType(vd.tpt))
    case _                                => Nil
  }

  /** Is tpt a by-name parameter type of the form => T? */
  def isByNameParamType(tpt: Tree) = tpt match {
    case TypeTree()                                                 => definitions.isByNameParamType(tpt.tpe)
    case AppliedTypeTree(Select(_, tpnme.BYNAME_PARAM_CLASS_NAME), _) => true
    case _                                                          => false
  }

  /** Is name a left-associative operator? */
  def isLeftAssoc(operator: Name) = operator.nonEmpty && (operator.endChar != ':')

  private val reserved = Set[Name](nme.false_, nme.true_, nme.null_)

  /** Is name a variable name? */
  def isVariableName(name: Name): Boolean = {
    val first = name(0)
    ((first.isLower && first.isLetter) || first == '_') && !reserved(name)
  }

  /** Is tree a `this` node which belongs to `enclClass`? */
  def isSelf(tree: Tree, enclClass: Symbol): Boolean = tree match {
    case This(_) => tree.symbol == enclClass
    case _ => false
  }

  /** can this type be a type pattern */
  def mayBeTypePat(tree: Tree): Boolean = tree match {
    case CompoundTypeTree(Template(tps, _, Nil)) => tps exists mayBeTypePat
    case Annotated(_, tp)                        => mayBeTypePat(tp)
    case AppliedTypeTree(constr, args)           => mayBeTypePat(constr) || args.exists(_.isInstanceOf[Bind])
    case SelectFromTypeTree(tp, _)               => mayBeTypePat(tp)
    case _                                       => false
  }

  /** Is this argument node of the form <expr> : _* ?
   */
  def isWildcardStarArg(tree: Tree): Boolean = tree match {
    case Typed(_, Ident(tpnme.WILDCARD_STAR)) => true
    case _                                  => false
  }
  
  /** Does this argument list end with an argument of the form <expr> : _* ? */
  def isWildcardStarArgList(trees: List[Tree]) =
    trees.nonEmpty && isWildcardStarArg(trees.last)
  
  /** Is the argument a wildcard argument of the form `_` or `x @ _`?
   */
  def isWildcardArg(tree: Tree): Boolean = unbind(tree) match {
    case Ident(nme.WILDCARD) => true
    case _                   => false
  }

  /** Is this pattern node a catch-all (wildcard or variable) pattern? */
  def isDefaultCase(cdef: CaseDef) = cdef match {
    case CaseDef(pat, EmptyTree, _) => isWildcardArg(pat)
    case _                          => false
  }
  
  /** Does this CaseDef catch Throwable? */
  def catchesThrowable(cdef: CaseDef) = catchesAllOf(cdef, ThrowableClass.tpe)
  
  /** Does this CaseDef catch everything of a certain Type? */
  def catchesAllOf(cdef: CaseDef, threshold: Type) =
    isDefaultCase(cdef) || (cdef.guard.isEmpty && (unbind(cdef.pat) match {
      case Typed(Ident(nme.WILDCARD), tpt)  => (tpt.tpe != null) && (threshold <:< tpt.tpe)
      case _                                => false
    }))

  /** Is this pattern node a catch-all or type-test pattern? */
  def isCatchCase(cdef: CaseDef) = cdef match {
    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), EmptyTree, _) => 
      isSimpleThrowable(tpt.tpe)
    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), EmptyTree, _) => 
      isSimpleThrowable(tpt.tpe)
    case _ => 
      isDefaultCase(cdef)
  }

  private def isSimpleThrowable(tp: Type): Boolean = tp match {
    case TypeRef(pre, sym, args) =>
      (pre == NoPrefix || pre.widen.typeSymbol.isStatic) &&
      (sym isNonBottomSubClass ThrowableClass) &&  /* bq */ !sym.isTrait
    case _ =>
      false
  }

  /* If we have run-time types, and these are used for pattern matching, 
     we should replace this  by something like:

      tp match {
        case TypeRef(pre, sym, args) =>
          args.isEmpty && (sym.owner.isPackageClass || isSimple(pre))
        case NoPrefix =>
          true
        case _ =>
          false
      }
*/

  /** Is this pattern node a sequence-valued pattern? */
  def isSequenceValued(tree: Tree): Boolean = unbind(tree) match {
    case Alternative(ts)            => ts exists isSequenceValued
    case ArrayValue(_, _) | Star(_) => true
    case _                          => false
  }
  
  /** The underlying pattern ignoring any bindings */
  def unbind(x: Tree): Tree = x match {
    case Bind(_, y) => unbind(y)
    case y          => y
  }
  
  /** Is this tree a Star(_) after removing bindings? */
  def isStar(x: Tree) = unbind(x) match { 
    case Star(_)  => true
    case _        => false
  }

  /** The method part of an application node
   */
  def methPart(tree: Tree): Tree = tree match {
    case Apply(fn, _)           => methPart(fn)
    case TypeApply(fn, _)       => methPart(fn)
    case AppliedTypeTree(fn, _) => methPart(fn)
    case _                      => tree
  }

  /** The depth of the nested applies: e.g. Apply(Apply(Apply(_, _), _), _)
   *  has depth 3.  Continues through type applications (without counting them.)
   */
  def applyDepth(tree: Tree): Int = tree match {
    case Apply(fn, _)           => 1 + applyDepth(fn)
    case TypeApply(fn, _)       => applyDepth(fn)
    case AppliedTypeTree(fn, _) => applyDepth(fn)
    case _                      => 0
  }
  def firstArgument(tree: Tree): Tree = tree match {
    case Apply(fn, args) => 
      val f = firstArgument(fn)
      if (f == EmptyTree && !args.isEmpty) args.head else f
    case _ =>
      EmptyTree
  }
  
  /** Is the tree Predef, scala.Predef, or _root_.scala.Predef?
   */
  def isPredefExpr(t: Tree) = t match {
    case Ident(nme.Predef)                                          => true
    case Select(Ident(nme.scala_), nme.Predef)                      => true
    case Select(Select(Ident(nme.ROOTPKG), nme.scala_), nme.Predef) => true
    case _                                                          => false
  }

  /** Does list of trees start with a definition of 
   *  a class of module with given name (ignoring imports)
   */
  def firstDefinesClassOrObject(trees: List[Tree], name: Name): Boolean = trees match {
      case Import(_, _) :: xs               => firstDefinesClassOrObject(xs, name)
      case Annotated(_, tree1) :: Nil       => firstDefinesClassOrObject(List(tree1), name)
      case ModuleDef(_, `name`, _) :: Nil   => true
      case ClassDef(_, `name`, _, _) :: Nil => true
      case _                                => false
    }
   
   
  /** Is this file the body of a compilation unit which should not
   *  have Predef imported?
   */
  def noPredefImportForUnit(body: Tree) = {
    // Top-level definition whose leading imports include Predef.
    def containsLeadingPredefImport(defs: List[Tree]): Boolean = defs match {
      case PackageDef(_, defs1) :: _ => containsLeadingPredefImport(defs1)
      case Import(expr, _) :: rest   => isPredefExpr(expr) || containsLeadingPredefImport(rest)
      case _                         => false
    }
    
    // Compilation unit is class or object 'name' in package 'scala'
    def isUnitInScala(tree: Tree, name: Name) = tree match {
      case PackageDef(Ident(nme.scala_), defs) => firstDefinesClassOrObject(defs, name)
      case _                                   => false
    }
    
    (  isUnitInScala(body, nme.Predef)
    || isUnitInScala(body, tpnme.ScalaObject)
    || containsLeadingPredefImport(List(body)))
  }

  def isAbsTypeDef(tree: Tree) = tree match {
    case TypeDef(_, _, _, TypeBoundsTree(_, _)) => true
    case TypeDef(_, _, _, rhs) => rhs.tpe.isInstanceOf[TypeBounds]
    case _ => false
  }

  def isAliasTypeDef(tree: Tree) = tree match {
    case TypeDef(_, _, _, _) => !isAbsTypeDef(tree)
    case _ => false
  }
  
  /** Some handy extractors for spotting trees through the
   *  the haze of irrelevant braces: i.e. Block(Nil, SomeTree)
   *  should not keep us from seeing SomeTree.
   */
  abstract class SeeThroughBlocks[T] {
    protected def unapplyImpl(x: Tree): T
    def unapply(x: Tree): T = x match {
      case Block(Nil, expr)         => unapply(expr)
      case _                        => unapplyImpl(x)
    }
  }
  object IsTrue extends SeeThroughBlocks[Boolean] {
    protected def unapplyImpl(x: Tree): Boolean = x match {
      case Literal(Constant(true)) => true
      case _                       => false
    }
  }
  object IsFalse extends SeeThroughBlocks[Boolean] {
    protected def unapplyImpl(x: Tree): Boolean = x match {
      case Literal(Constant(false)) => true
      case _                        => false
    }
  }
  object IsIf extends SeeThroughBlocks[Option[(Tree, Tree, Tree)]] {
    protected def unapplyImpl(x: Tree) = x match {
      case If(cond, thenp, elsep) => Some((cond, thenp, elsep))
      case _                      => None
    }
  }
}
