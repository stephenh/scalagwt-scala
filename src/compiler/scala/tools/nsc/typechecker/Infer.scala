/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.util.control.ControlThrowable
import scala.tools.util.StringOps.{ countAsString, countElementsAsString }
import symtab.Flags._
import scala.annotation.tailrec

/** This trait ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait Infer {
  self: Analyzer =>

  import global._
  import definitions._
  import typer.printInference
  import typeDebug.ptBlock

/* -- Type parameter inference utility functions --------------------------- */

  private def assertNonCyclic(tvar: TypeVar) =
    assert(tvar.constr.inst != tvar, tvar.origin)

  /** The formal parameter types corresponding to <code>formals</code>.
   *  If <code>formals</code> has a repeated last parameter, a list of
   *  (nargs - params.length + 1) copies of its type is returned.
   *  By-name types are replaced with their underlying type.
   *
   *  @param removeByName allows keeping ByName parameters. Used in NamesDefaults.
   *  @param removeRepeated allows keeping repeated parameter (if there's one argument). Used in NamesDefaults.
   */
  def formalTypes(formals: List[Type], nargs: Int, removeByName: Boolean = true, removeRepeated: Boolean = true): List[Type] = {
    val formals1 = if (removeByName) formals mapConserve {
      case TypeRef(_, ByNameParamClass, List(arg)) => arg
      case formal => formal
    } else formals
    if (isVarArgTypes(formals1) && (removeRepeated || formals.length != nargs)) {
      val ft = formals1.last.normalize.typeArgs.head
      formals1.init ::: (for (i <- List.range(formals1.length - 1, nargs)) yield ft)
    } else formals1
  }

  def actualTypes(actuals: List[Type], nformals: Int): List[Type] =
    if (nformals == 1 && !hasLength(actuals, 1))
      List(if (actuals.isEmpty) UnitClass.tpe else tupleType(actuals))
    else actuals

  def actualArgs(pos: Position, actuals: List[Tree], nformals: Int): List[Tree] = {
    val inRange = nformals == 1 && !hasLength(actuals, 1) && actuals.lengthCompare(MaxTupleArity) <= 0
    if (inRange && !phase.erasedTypes) List(atPos(pos)(gen.mkTuple(actuals)))
    else actuals
  }

  /** A fresh type variable with given type parameter as origin.
   *
   *  @param tparam ...
   *  @return       ...
   */
  def freshVar(tparam: Symbol): TypeVar = TypeVar(tparam) 
  
  private class NoInstance(msg: String) extends Throwable(msg) with ControlThrowable { }
  private class DeferredNoInstance(getmsg: () => String) extends NoInstance("") {
    override def getMessage(): String = getmsg()
  }
  private def ifNoInstance[T](f: String => T): PartialFunction[Throwable, T] = {
    case x: NoInstance  => f(x.getMessage)
  }

  /** Map every TypeVar to its constraint.inst field.
   *  throw a NoInstance exception if a NoType or WildcardType is encountered.
   */
  object instantiate extends TypeMap {
    private var excludedVars = immutable.Set[TypeVar]()
    def apply(tp: Type): Type = tp match {
      case WildcardType | BoundedWildcardType(_) | NoType =>
        throw new NoInstance("undetermined type")
      case tv @ TypeVar(origin, constr) =>
        if (constr.inst == NoType) {
          throw new DeferredNoInstance(() =>
            "no unique instantiation of type variable " + origin + " could be found")
        } else if (excludedVars(tv)) {
          throw new NoInstance("cyclic instantiation")
        } else {
          excludedVars += tv
          val res = apply(constr.inst)
          excludedVars -= tv
          res
        } 
      case _ =>
        mapOver(tp)
    }
  }

  /** Is type fully defined, i.e. no embedded anytypes or wildcards in it?
   *
   *  @param tp ...
   *  @return   ...
   */
  private[typechecker] def isFullyDefined(tp: Type): Boolean = tp match {
    case WildcardType | BoundedWildcardType(_) | NoType =>
      false
    case NoPrefix | ThisType(_) | ConstantType(_) => 
      true
    case TypeRef(pre, sym, args) =>
      isFullyDefined(pre) && (args forall isFullyDefined)
    case SingleType(pre, sym) =>
      isFullyDefined(pre)
    case RefinedType(ts, decls) =>
      ts forall isFullyDefined
    case TypeVar(origin, constr) if (constr.inst == NoType) =>
      false
    case _ =>
      try {
        instantiate(tp); true
      } catch {
        case ex: NoInstance => false
      }
  }        

  /** Solve constraint collected in types `tvars`.
   *
   *  @param tvars      All type variables to be instantiated.
   *  @param tparams    The type parameters corresponding to `tvars`
   *  @param variances  The variances of type parameters; need to reverse
   *                    solution direction for all contravariant variables.
   *  @param upper      When `true` search for max solution else min.
   *  @throws NoInstance
   */
  def solvedTypes(tvars: List[TypeVar], tparams: List[Symbol],
                  variances: List[Int], upper: Boolean, depth: Int): List[Type] = {

    if (!solve(tvars, tparams, variances, upper, depth)) {
      // no panic, it's good enough to just guess a solution, we'll find out
      // later whether it works.  *ZAP* @M danger, Will Robinson! this means
      // that you should never trust inferred type arguments!
      //
      // Need to call checkBounds on the args/typars or type1 on the tree
      // for the expression that results from type inference see e.g., #2421:
      // implicit search had been ignoring this caveat
      // throw new DeferredNoInstance(() =>
      //   "no solution exists for constraints"+(tvars map boundsString))
    }
    for (tvar <- tvars ; if tvar.constr.inst == tvar) {
      if (tvar.origin.typeSymbol.info eq ErrorType)
        // this can happen if during solving a cyclic type parameter
        // such as T <: T gets completed. See #360
        tvar.constr.inst = ErrorType
      else
        assert(false, tvar.origin+" at "+tvar.origin.typeSymbol.owner)
    }
    tvars map instantiate
  }

  def skipImplicit(tp: Type) = tp match {
    case mt: MethodType if mt.isImplicit  => mt.resultType
    case _                                => tp
  }

  /** Automatically perform the following conversions on expression types:
   *  A method type becomes the corresponding function type.
   *  A nullary method type becomes its result type.
   *  Implicit parameters are skipped.
   *  This method seems to be performance critical.
   */
  def normalize(tp: Type): Type = tp match {
    case mt @ MethodType(params, restpe) if mt.isImplicit =>
      normalize(restpe)
    case mt @ MethodType(params, restpe) if !restpe.isDependent =>
      functionType(params map (_.tpe), normalize(restpe))
    case NullaryMethodType(restpe) =>
      normalize(restpe)
    case ExistentialType(tparams, qtpe) =>
      ExistentialType(tparams, normalize(qtpe))
    case tp1 =>
      tp1 // @MAT aliases already handled by subtyping
  }

  private val stdErrorClass = RootClass.newErrorClass(tpnme.ERROR)
  private val stdErrorValue = stdErrorClass.newErrorValue(nme.ERROR)

  /** The context-dependent inferencer part */
  class Inferencer(context: Context) extends InferencerErrorTrees {
    /* -- Error Messages --------------------------------------------------- */
    def setError[T <: Tree](tree: T): T = {
      def name        = newTermName("<error: " + tree.symbol + ">")
      def errorClass  = if (context.reportGeneralErrors) context.owner.newErrorClass(name.toTypeName) else stdErrorClass
      def errorValue  = if (context.reportGeneralErrors) context.owner.newErrorValue(name) else stdErrorValue
      def errorSym    = if (tree.isType) errorClass else errorValue
      
      if (tree.hasSymbol)
        tree setSymbol errorSym

      tree setType ErrorType
    }
    
    protected def getContext = context

    def error(pos: Position, msg: String) {
      context.error(pos, msg)
    }

/*    def makeErrorTree(tree: Tree, msg: String): Tree = {
      if (!tree.isErroneous) error(tree.pos, msg)
      setError(tree)
    }*/

    def typeError(pos: Position, found: Type, req: Type) {
      if (!found.isErroneous && !req.isErroneous) {
        error(pos, withAddendum(pos)(typeErrorMsg(found, req)))
        if (settings.explaintypes.value)
          explainTypes(found, req)
      }
    }

    def typeErrorMsg(found: Type, req: Type) = {
      def isPossiblyMissingArgs = (found.resultApprox ne found) && isWeaklyCompatible(found.resultApprox, req)
      def missingArgsMsg = if (isPossiblyMissingArgs) "\n possible cause: missing arguments for method or constructor" else ""
      
      "type mismatch" + foundReqMsg(found, req) + missingArgsMsg
    }

    // TODO: replace with standard Error tree
    // currently only used in adapt in Typers
    def typeErrorTree(tree: Tree, found: Type, req: Type): Tree = {
      // If the expected type is a refinement type, and the found type is a refinement or an anon
      // class, we can greatly improve the error message by retyping the tree to recover the actual
      // members present, then display along with the expected members. This is done here because
      // this is the last point where we still have access to the original tree, rather than just
      // the found/req types.
      val foundType: Type = req.normalize match {
        case RefinedType(parents, decls) if !decls.isEmpty && found.typeSymbol.isAnonOrRefinementClass =>
          val retyped    = typer typed (tree.duplicate setType null)
          val foundDecls = retyped.tpe.decls filter (sym => !sym.isConstructor && !sym.isSynthetic)
          
          if (foundDecls.isEmpty) found
          else {
            // The members arrive marked private, presumably because there was no
            // expected type and so they're considered members of an anon class.
            foundDecls foreach (_ resetFlag (PRIVATE | PROTECTED))
            // TODO: if any of the found parents match up with required parents after normalization,
            // print the error so that they match. The major beneficiary there would be
            // java.lang.Object vs. AnyRef.
            refinedType(found.parents, found.typeSymbol.owner, foundDecls, tree.pos)
          }
        case _ =>
          found
      }
      typeError(tree.pos, foundType, req)
      setError(tree)
    }

    def explainTypes(tp1: Type, tp2: Type) = 
      withDisambiguation(List(), tp1, tp2)(global.explainTypes(tp1, tp2))

    /* -- Tests & Checks---------------------------------------------------- */

    /** Check that <code>sym</code> is defined and accessible as a member of
     *  tree <code>site</code> with type <code>pre</code> in current context.
     *
     * Note: pre is not refchecked -- moreover, refchecking the resulting tree may not refcheck pre,
     *       since pre may not occur in its type (callers should wrap the result in a TypeTreeWithDeferredRefCheck)
     */
    def checkAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): Tree =
      if (sym.isError) {
        tree setSymbol sym setType ErrorType
      } else {
        val topClass = context.owner.toplevelClass
        if (context.unit.exists)
          context.unit.depends += sym.toplevelClass 

        var sym1 = sym filter (alt => context.isAccessible(alt, pre, site.isInstanceOf[Super]))
        // Console.println("check acc " + (sym, sym1) + ":" + (sym.tpe, sym1.tpe) + " from " + pre);//DEBUG
        
        if (sym1 == NoSymbol && sym.isJavaDefined && context.unit.isJava) // don't try to second guess Java; see #4402
          sym1 = sym

        if (sym1 == NoSymbol) {
          if (settings.debug.value) {
            Console.println(context)
            Console.println(tree)
            Console.println("" + pre + " " + sym.owner + " " + context.owner + " " + context.outer.enclClass.owner + " " + sym.owner.thisType + (pre =:= sym.owner.thisType))
          }
          AccessError(tree, sym, pre, context.enclClass.owner,
            if (settings.check.isDefault)
              analyzer.lastAccessCheckDetails
            else
              ptBlock("because of an internal error (no accessible symbol)",
                "sym.ownerChain"                -> sym.ownerChain,
                "underlying(sym)"               -> underlying(sym),
                "pre"                           -> pre,
                "site"                          -> site,
                "tree"                          -> tree,
                "sym.accessBoundary(sym.owner)" -> sym.accessBoundary(sym.owner),
                "context.owner"                 -> context.owner,
                "context.outer.enclClass.owner" -> context.outer.enclClass.owner
              )
          )
        }
        else {
          if(sym1.isTerm)
            sym1.cookJavaRawInfo() // xform java rawtypes into existentials

          var owntype = try{ 
            pre.memberType(sym1)
          } catch {
            case ex: MalformedType =>
              if (settings.debug.value) ex.printStackTrace
              val sym2 = underlying(sym1)
              val itype = pre.memberType(sym2)
              return AccessError(tree, sym, pre, context.enclClass.owner,
                          "\n because its instance type "+itype+
                          (if ("malformed type: "+itype.toString==ex.msg) " is malformed" 
                           else " contains a "+ex.msg))

// enabling the following TypeError case
// crashes a few examples because there are situations (like in NamesDefaults)
// where CyclicReference is expected
//            case ex: TypeError =>
//              return TypeErrorTree(tree, ex)(context)
              
          }
          if (pre.isInstanceOf[SuperType])
            owntype = owntype.substSuper(pre, site.symbol.thisType)
          tree setSymbol sym1 setType owntype
        }
      }

    /** Capturing the overlap between isPlausiblyCompatible and normSubType.
     *  This is a faithful translation of the code which was there, but it
     *  seems likely the methods are intended to be even more similar than
     *  they are: perhaps someone more familiar with the intentional distinctions
     *  can examine the now much smaller concrete implementations below.
     */
/*
    abstract class CompatibilityChecker {
      def resultTypeCheck(restpe: Type, arg: Type): Boolean
      def argumentCheck(arg: Type, param: Type): Boolean
      def lastChanceCheck(tp: Type, pt: Type): Boolean
      
      final def mtcheck(tp: MethodType, pt: TypeRef): Boolean = {
        val MethodType(params, restpe) = tp
        val TypeRef(pre, sym, args) = pt

        if (sym.isAliasType) apply(tp, pt.normalize)
        else if (sym.isAbstractType) apply(tp, pt.bounds.lo)
        else {
          val len = args.length - 1
          hasLength(params, len) &&
          sym == FunctionClass(len) && {
            val ps = params.iterator
            val as = args.iterator
            while (ps.hasNext && as.hasNext) {
              if (!argumentCheck(as.next, ps.next.tpe))
                return false
            }
            ps.isEmpty && as.hasNext && {
              val lastArg = as.next
              as.isEmpty && resultTypeCheck(restpe, lastArg)
            }
          }
        }
      }

      def apply(tp: Type, pt: Type): Boolean = tp match {
        case mt @ MethodType(_, restpe) =>
          if (mt.isImplicit)
            apply(restpe, pt)
          else pt match {
            case tr: TypeRef  => mtcheck(mt, tr)
            case _            => lastChanceCheck(tp, pt)
          }
        case NullaryMethodType(restpe)  => apply(restpe, pt)
        case PolyType(_, restpe)        => apply(restpe, pt)
        case ExistentialType(_, qtpe)   => apply(qtpe, pt)
        case _                          => argumentCheck(tp, pt)
      }
    }
    
    object isPlausiblyCompatible extends CompatibilityChecker {
      def resultTypeCheck(restpe: Type, arg: Type) = isPlausiblyCompatible(restpe, arg)
      def argumentCheck(arg: Type, param: Type)    = isPlausiblySubType(arg, param)
      def lastChanceCheck(tp: Type, pt: Type)      = false
    }
    object normSubType extends CompatibilityChecker {
      def resultTypeCheck(restpe: Type, arg: Type) = normSubType(restpe, arg)
      def argumentCheck(arg: Type, param: Type)    = arg <:< param
      def lastChanceCheck(tp: Type, pt: Type)      = tp <:< pt
      
      override def apply(tp: Type, pt: Type): Boolean = tp match {
        case ExistentialType(_, _)     => normalize(tp) <:< pt
        case _                         => super.apply(tp, pt)
      }
    }
*/
    def isPlausiblyCompatible(tp: Type, pt: Type) = checkCompatibility(true, tp, pt)
    def normSubType(tp: Type, pt: Type) = checkCompatibility(false, tp, pt)

    @tailrec private def checkCompatibility(fast: Boolean, tp: Type, pt: Type): Boolean = tp match {
      case mt @ MethodType(params, restpe) =>
        if (mt.isImplicit)
          checkCompatibility(fast, restpe, pt)
        else pt match {
          case tr @ TypeRef(pre, sym, args) => 

            if (sym.isAliasType) checkCompatibility(fast, tp, pt.normalize)
            else if (sym.isAbstractType) checkCompatibility(fast, tp, pt.bounds.lo)
            else {
              val len = args.length - 1
              hasLength(params, len) &&
              sym == FunctionClass(len) && {
                var ps = params
                var as = args
                if (fast) {
                  while (ps.nonEmpty && as.nonEmpty) {
                    if (!isPlausiblySubType(as.head, ps.head.tpe))
                      return false
                    ps = ps.tail
                    as = as.tail
                  }
                } else {
                  while (ps.nonEmpty && as.nonEmpty) {
                    if (!(as.head <:< ps.head.tpe))
                      return false
                    ps = ps.tail
                    as = as.tail
                  }
                }
                ps.isEmpty && as.nonEmpty && {
                  val lastArg = as.head
                  as.tail.isEmpty && checkCompatibility(fast, restpe, lastArg)
                }
              }
            }
          
          case _            => if (fast) false else tp <:< pt
        }
      case NullaryMethodType(restpe)  => checkCompatibility(fast, restpe, pt)
      case PolyType(_, restpe)        => checkCompatibility(fast, restpe, pt)
      case ExistentialType(_, qtpe)   => if (fast) checkCompatibility(fast, qtpe, pt) else normalize(tp) <:< pt // is !fast case needed??
      case _                          => if (fast) isPlausiblySubType(tp, pt) else tp <:< pt
    }


    /** This expresses more cleanly in the negative: there's a linear path
     *  to a final true or false.
     */
    private def isPlausiblySubType(tp1: Type, tp2: Type) = !isImpossibleSubType(tp1, tp2)
    private def isImpossibleSubType(tp1: Type, tp2: Type) = tp1.normalize.widen match {
      case tr1 @ TypeRef(_, sym1, _) =>
        // We can only rule out a subtype relationship if the left hand
        // side is a class, else we may not know enough.
        sym1.isClass && (tp2.normalize.widen match {
          case TypeRef(_, sym2, _) =>
             sym2.isClass &&
            !(sym1 isSubClass sym2) &&
            !(sym1 isNumericSubClass sym2)
          case RefinedType(parents, decls) =>
            decls.nonEmpty &&
            tr1.member(decls.head.name) == NoSymbol
          case _ => false
        })
      case _ => false
    }

    def isCompatible(tp: Type, pt: Type): Boolean = {
      val tp1 = normalize(tp)
      (tp1 weak_<:< pt) || isCoercible(tp1, pt)
    }
    def isCompatibleArgs(tps: List[Type], pts: List[Type]) = 
      (tps corresponds pts)(isCompatible)

    def isWeaklyCompatible(tp: Type, pt: Type): Boolean =
      pt.typeSymbol == UnitClass || // can perform unit coercion
      isCompatible(tp, pt) ||
      tp.isInstanceOf[MethodType] && // can perform implicit () instantiation
      tp.params.isEmpty && isCompatible(tp.resultType, pt)

    /** Like weakly compatible but don't apply any implicit conversions yet.
     *  Used when comparing the result type of a method with its prototype.
     */
    def isConservativelyCompatible(tp: Type, pt: Type): Boolean =
      context.withImplicitsDisabled(isWeaklyCompatible(tp, pt))

    /** This is overridden in the Typer.infer with some logic, but since
     *  that's the only place in the compiler an Inferencer is ever created,
     *  I suggest this should either be abstract or have the implementation.
     */
    def isCoercible(tp: Type, pt: Type): Boolean = false

    /* -- Type instantiation------------------------------------------------ */

    /** Replace any (possibly bounded) wildcard types in type `tp`
     *  by existentially bound variables.
     */
    def makeFullyDefined(tp: Type): Type = {
      val tparams = new ListBuffer[Symbol]
      def addTypeParam(bounds: TypeBounds): Type = {
        val tparam =  
          context.owner.newAbstractType(context.tree.pos.focus, newTypeName("_"+tparams.size))
            .setFlag(EXISTENTIAL)
            .setInfo(bounds)
        tparams += tparam
        tparam.tpe
      }
      val tp1 = tp map { 
        case WildcardType =>
          addTypeParam(TypeBounds.empty)
        case BoundedWildcardType(bounds) =>
          addTypeParam(bounds)
        case t => t
      }
      existentialAbstraction(tparams.toList, tp1)
    }

    /** Return inferred type arguments of polymorphic expression, given 
     *  its type parameters and result type and a prototype <code>pt</code>.
     *  If no minimal type variables exist that make the
     *  instantiated type a subtype of <code>pt</code>, return null.
     *
     *  @param tparams ...
     *  @param restpe  ...
     *  @param pt      ...
     *  @return        ...
     */
    private def exprTypeArgs(tparams: List[Symbol], restpe: Type, pt: Type, useWeaklyCompatible: Boolean = false): List[Type] = {
      val tvars = tparams map freshVar
      val instResTp = restpe.instantiateTypeParams(tparams, tvars)
      if ( if (useWeaklyCompatible) isWeaklyCompatible(instResTp, pt) else isCompatible(instResTp, pt) ) {
        try {
          // If the restpe is an implicit method, and the expected type is fully defined
          // optimize type variables wrt to the implicit formals only; ignore the result type.
          // See test pos/jesper.scala 
          val varianceType = restpe match { 
            case mt: MethodType if mt.isImplicit && isFullyDefined(pt) =>
              MethodType(mt.params, AnyClass.tpe)
            case _ =>
              restpe
          }
          //println("try to solve "+tvars+" "+tparams)
          solvedTypes(tvars, tparams, tparams map varianceInType(varianceType), 
                      false, lubDepth(List(restpe, pt)))
        } catch {
          case ex: NoInstance => null
        }
      } else null
    }

    /** Return inferred proto-type arguments of function, given
    *  its type and value parameters and result type, and a
    *  prototype <code>pt</code> for the function result.
    *  Type arguments need to be either determined precisely by
    *  the prototype, or they are maximized, if they occur only covariantly
    *  in the value parameter list.
    *  If instantiation of a type parameter fails, 
    *  take WildcardType for the proto-type argument.
    *
    *  @param tparams ...
    *  @param formals ...
    *  @param restype ...
    *  @param pt      ...
    *  @return        ...
    */
    def protoTypeArgs(tparams: List[Symbol], formals: List[Type], restpe: Type,
                      pt: Type): List[Type] = {
      /** Map type variable to its instance, or, if `variance` is covariant/contravariant,
       *  to its upper/lower bound */
      def instantiateToBound(tvar: TypeVar, variance: Int): Type = try {
        lazy val hiBounds = tvar.constr.hiBounds
        lazy val loBounds = tvar.constr.loBounds
        lazy val upper = glb(hiBounds)
        lazy val lower = lub(loBounds)
        def setInst(tp: Type): Type = {
          tvar setInst tp
          assertNonCyclic(tvar)//debug
          instantiate(tvar.constr.inst)
        }
        //Console.println("instantiate "+tvar+tvar.constr+" variance = "+variance);//DEBUG
        if (tvar.constr.inst != NoType) 
          instantiate(tvar.constr.inst)
        else if ((variance & COVARIANT) != 0 && hiBounds.nonEmpty)
          setInst(upper)
        else if ((variance & CONTRAVARIANT) != 0 && loBounds.nonEmpty)
          setInst(lower)
        else if (hiBounds.nonEmpty && loBounds.nonEmpty && upper <:< lower)
          setInst(upper)
        else
          WildcardType
      } catch {
        case ex: NoInstance => WildcardType
      }
      val tvars = tparams map freshVar
      if (isConservativelyCompatible(restpe.instantiateTypeParams(tparams, tvars), pt))
        (tparams, tvars).zipped map ((tparam, tvar) =>
          instantiateToBound(tvar, varianceInTypes(formals)(tparam)))
      else 
        tvars map (tvar => WildcardType)
    }

    object AdjustedTypeArgs {
      type Result = collection.mutable.LinkedHashMap[Symbol, Option[Type]]

      def unapply(m: Result): Some[(List[Symbol], List[Type])] = Some(toLists(
        m collect {case (p, Some(a)) => (p, a)} unzip  ))

      object Undets {
        def unapply(m: Result): Some[(List[Symbol], List[Type], List[Symbol])] = Some(toLists{
          val (ok, nok) = m.map{case (p, a) => (p, a.getOrElse(null))}.partition(_._2 ne null)
          val (okArgs, okTparams) = ok.unzip
          (okArgs, okTparams, nok.keys)
        })
      }

      object AllArgsAndUndets {
        def unapply(m: Result): Some[(List[Symbol], List[Type], List[Type], List[Symbol])] = Some(toLists{
          val (ok, nok) = m.map{case (p, a) => (p, a.getOrElse(null))}.partition(_._2 ne null)
          val (okArgs, okTparams) = ok.unzip
          (okArgs, okTparams, m.values.map(_.getOrElse(NothingClass.tpe)), nok.keys)
        })
      }

      @inline private def toLists[A1, A2](pxs: (Iterable[A1], Iterable[A2])) = (pxs._1.toList, pxs._2.toList)
      @inline private def toLists[A1, A2, A3](pxs: (Iterable[A1], Iterable[A2], Iterable[A3])) = (pxs._1.toList, pxs._2.toList, pxs._3.toList)
      @inline private def toLists[A1, A2, A3, A4](pxs: (Iterable[A1], Iterable[A2], Iterable[A3], Iterable[A4])) = (pxs._1.toList, pxs._2.toList, pxs._3.toList, pxs._4.toList)
    }

    /** Retract arguments that were inferred to Nothing because inference failed. Correct types for repeated params.
     *
     * We detect Nothing-due-to-failure by only retracting a parameter if either:
     *  - it occurs in an invariant/contravariant position in `restpe`
     *  - `restpe == WildcardType`
     *
     * Retracted parameters are mapped to None. 
     *  TODO: 
     *    - make sure the performance hit of storing these in a map is acceptable (it's going to be a small map in 90% of the cases, I think)
     *    - refactor further up the callstack so that we don't have to do this post-factum adjustment?
     *
     * Rewrite for repeated param types:  Map T* entries to Seq[T].
     *  @return map from tparams to inferred arg, if inference was successful, tparams that map to None are considered left undetermined
     *    type parameters that are inferred as `scala.Nothing` and that are not covariant in <code>restpe</code> are taken to be undetermined
     */
    def adjustTypeArgs(tparams: List[Symbol], targs: List[Type], restpe: Type = WildcardType): AdjustedTypeArgs.Result  = {
      @inline def notCovariantIn(tparam: Symbol, restpe: Type) =
        (varianceInType(restpe)(tparam) & COVARIANT) == 0  // tparam occurred non-covariantly (in invariant or contravariant position)

      (tparams, targs).zipped.map{ (tparam, targ) =>
        if (targ.typeSymbol == NothingClass && 
            (restpe.isWildcard || notCovariantIn(tparam, restpe))) {
          tparam -> None
        } else {
          tparam -> Some(
            if      (targ.typeSymbol == RepeatedParamClass)     targ.baseType(SeqClass)
            else if (targ.typeSymbol == JavaRepeatedParamClass) targ.baseType(ArrayClass)
            else if (targ.typeSymbol.isModuleClass) targ  // this infers Foo.type instead of "object Foo" (see also widenIfNecessary)
            else targ.widen
          )
        }
      }(collection.breakOut)
    }
    
    /** Return inferred type arguments, given type parameters, formal parameters,
    *  argument types, result type and expected result type.
    *  If this is not possible, throw a <code>NoInstance</code> exception.
    *  Undetermined type arguments are represented by `definitions.NothingClass.tpe`.
    *  No check that inferred parameters conform to their bounds is made here.
    *
    *  @param   tparams         the type parameters of the method
    *  @param   formals         the value parameter types of the method
    *  @param   restp           the result type of the method
    *  @param   argtpes         the argument types of the application
    *  @param   pt              the expected return type of the application
    *  @return  @see adjustTypeArgs
    *
    *  @throws                  NoInstance
    */
    def methTypeArgs(tparams: List[Symbol], formals: List[Type], restpe: Type, 
                     argtpes: List[Type], pt: Type): AdjustedTypeArgs.Result = {
      val tvars = tparams map freshVar
      if (!sameLength(formals, argtpes))
        throw new NoInstance("parameter lists differ in length")
      
      val restpeInst = restpe.instantiateTypeParams(tparams, tvars)
      printInference(
        ptBlock("methTypeArgs",
          "tparams"     -> tparams,
          "formals"     -> formals,
          "restpe"      -> restpe,
          "restpeInst"  -> restpeInst,
          "argtpes"     -> argtpes,
          "pt"          -> pt,
          "tvars"       -> tvars,
          "constraints" -> tvars.map(_.constr)
        )
      )

      // first check if typevars can be fully defined from the expected type.
      // The return value isn't used so I'm making it obvious that this side
      // effects, because a function called "isXXX" is not the most obvious
      // side effecter.
      isConservativelyCompatible(restpeInst, pt)
      
      // Return value unused with the following explanation:
      //
      // Just wait and instantiate from the arguments.  That way,
      // we can try to apply an implicit conversion afterwards.
      // This case could happen if restpe is not fully defined, so the
      // search for an implicit from restpe => pt fails due to ambiguity.
      // See #347.  Therefore, the following two lines are commented out.
      //
      // throw new DeferredNoInstance(() =>
      //   "result type " + normalize(restpe) + " is incompatible with expected type " + pt)

      for (tvar <- tvars)
        if (!isFullyDefined(tvar)) tvar.constr.inst = NoType
 
      // Then define remaining type variables from argument types.
      (argtpes, formals).zipped map { (argtpe, formal) =>
        val tp1 = argtpe.deconst.instantiateTypeParams(tparams, tvars)
        val pt1 = formal.instantiateTypeParams(tparams, tvars)
        
        // Note that isCompatible side-effects: subtype checks involving typevars
        // are recorded in the typevar's bounds (see TypeConstraint)
        if (!isCompatible(tp1, pt1)) {
          throw new DeferredNoInstance(() =>
            "argument expression's type is not compatible with formal parameter type" + foundReqMsg(tp1, pt1))
        }
      }
      val targs = solvedTypes(
        tvars, tparams, tparams map varianceInTypes(formals), 
        false, lubDepth(formals) max lubDepth(argtpes)
      )
      val result = adjustTypeArgs(tparams, targs, restpe)

      printInference(
        ptBlock("methTypeArgs result",
          "tvars"              -> tvars,
          "constraints"        -> tvars.map(_.constr),
          "targs"              -> targs,
          "adjusted type args" -> result
        )
      )
      result   
    }

    private[typechecker] def followApply(tp: Type): Type = tp match {
      case NullaryMethodType(restp) => 
        val restp1 = followApply(restp)
        if (restp1 eq restp) tp else restp1
      case _ =>
        val appmeth = tp.nonPrivateMember(nme.apply) filter (_.isPublic)
        if (appmeth == NoSymbol) tp 
        else OverloadedType(tp, appmeth.alternatives)
    }

    def hasExactlyNumParams(tp: Type, n: Int): Boolean = tp match {
      case OverloadedType(pre, alts) =>
        alts exists (alt => hasExactlyNumParams(pre.memberType(alt), n))
      case _ =>      
        val len = tp.params.length
        len == n || isVarArgsList(tp.params) && len <= n + 1
    }

    /**
     * Verifies whether the named application is valid. The logic is very
     * similar to the one in NamesDefaults.removeNames.
     *
     * @return a triple (argtpes1, argPos, namesOk) where
     *  - argtpes1 the argument types in named application (assignments to
     *    non-parameter names are treated as assignments, i.e. type Unit)
     *  - argPos a Function1[Int, Int] mapping arguments from their current
     *    to the corresponding position in params
     *  - namesOK is false when there's an invalid use of named arguments
     */
    private def checkNames(argtpes: List[Type], params: List[Symbol]) = {
      val argPos = Array.fill(argtpes.length)(-1)
      var positionalAllowed, namesOK = true
      var index = 0
      val argtpes1 = argtpes map {
        case NamedType(name, tp) => // a named argument
          var res = tp
          val pos = params.indexWhere(p => paramMatchesName(p, name) && !p.isSynthetic)
                    
          if (pos == -1) {
            if (positionalAllowed) { // treat assignment as positional argument
              argPos(index) = index
              res = UnitClass.tpe
            } else                   // unknown parameter name
              namesOK = false
          } else if (argPos.contains(pos)) { // parameter specified twice
            namesOK = false
          } else {
            positionalAllowed = false
            argPos(index) = pos
          }
          index += 1
          res
        case tp => // a positional argument
          argPos(index) = index
          if (!positionalAllowed)
            namesOK = false // positional after named
          index += 1
          tp
      }
      (argtpes1, argPos, namesOK)
    }

    /** don't do a () to (()) conversion for methods whose second parameter
     * is a varargs. This is a fairly kludgey way to address #3224. 
     * We'll probably find a better way to do this by identifying
     * tupled and n-ary methods, but thiws is something for a future major revision.
     */
    def isUnitForVarArgs(args: List[AnyRef], params: List[Symbol]): Boolean = 
      args.isEmpty && hasLength(params, 2) && isVarArgsList(params)

    /** Is there an instantiation of free type variables <code>undetparams</code>
     *  such that function type <code>ftpe</code> is applicable to
     *  <code>argtpes</code> and its result conform to <code>pt</code>?
     *
     *  @param undetparams ...
     *  @param ftpe        the type of the function (often a MethodType)
     *  @param argtpes     the argument types; a NamedType(name, tp) for named
     *    arguments. For each NamedType, if `name` does not exist in `ftpe`, that
     *    type is set to `Unit`, i.e. the corresponding argument is treated as
     *    an assignment expression (@see checkNames).
     *  @param pt          ...
     *  @return            ...
     */
    private def isApplicable(undetparams: List[Symbol], ftpe: Type,
                             argtpes0: List[Type], pt: Type): Boolean =
      ftpe match {
        case OverloadedType(pre, alts) =>
          alts exists (alt => isApplicable(undetparams, pre.memberType(alt), argtpes0, pt))
        case ExistentialType(tparams, qtpe) =>
          isApplicable(undetparams, qtpe, argtpes0, pt)
        case MethodType(params, _) =>
          val formals0 = params map { param => 
            param.tpe match {
              case TypeRef(_, sym, List(tpe)) if sym isNonBottomSubClass CodeClass => tpe
              case tpe => tpe
            }
          }
          val formals = formalTypes(formals0, argtpes0.length)

          def tryTupleApply: Boolean = {
            // if 1 formal, 1 argtpe (a tuple), otherwise unmodified argtpes0
            val tupleArgTpes = actualTypes(argtpes0 map {
                // no assignment is treated as named argument here
              case NamedType(name, tp) => UnitClass.tpe
              case tp => tp
              }, formals.length)

            !sameLength(argtpes0, tupleArgTpes) &&
            !isUnitForVarArgs(argtpes0, params) &&
            isApplicable(undetparams, ftpe, tupleArgTpes, pt)
          }
          def typesCompatible(argtpes: List[Type]) = {
            val restpe = ftpe.resultType(argtpes)
            if (undetparams.isEmpty) {
              isCompatibleArgs(argtpes, formals) && isWeaklyCompatible(restpe, pt)
            } else {
              try {
                val AdjustedTypeArgs.Undets(okparams, okargs, leftUndet) = methTypeArgs(undetparams, formals, restpe, argtpes, pt)
                // #2665: must use weak conformance, not regular one (follow the monomorphic case above)
                (exprTypeArgs(leftUndet, restpe.instantiateTypeParams(okparams, okargs), pt, useWeaklyCompatible = true) ne null) && 
                isWithinBounds(NoPrefix, NoSymbol, okparams, okargs)
              } catch {
                case ex: NoInstance => false
              }
            }
          }

          // very similar logic to doTypedApply in typechecker
          val lencmp = compareLengths(argtpes0, formals)
          if (lencmp > 0) tryTupleApply
          else if (lencmp == 0) {
            if (!argtpes0.exists(_.isInstanceOf[NamedType])) {
              // fast track if no named arguments are used
              typesCompatible(argtpes0)
            }
            else {
              // named arguments are used
              val (argtpes1, argPos, namesOK) = checkNames(argtpes0, params)
              // when using named application, the vararg param has to be specified exactly once
              ( namesOK && (isIdentity(argPos) || sameLength(formals, params)) &&
              // nb. arguments and names are OK, check if types are compatible
                typesCompatible(reorderArgs(argtpes1, argPos))
              )
            }
          } else {
            // not enough arguments, check if applicable using defaults
            val missing = missingParams[Type](argtpes0, params, {
              case NamedType(name, _) => Some(name)
              case _ => None
            })._1
            if (missing forall (_.hasDefaultFlag)) {
              // add defaults as named arguments
              val argtpes1 = argtpes0 ::: (missing map (p => NamedType(p.name, p.tpe)))
              isApplicable(undetparams, ftpe, argtpes1, pt)
            }
            else tryTupleApply
          }

        case NullaryMethodType(restpe) => // strip nullary method type, which used to be done by the polytype case below
          isApplicable(undetparams, restpe, argtpes0, pt)
        case PolyType(tparams, restpe) =>
          val tparams1 = cloneSymbols(tparams)
          isApplicable(tparams1 ::: undetparams, restpe.substSym(tparams, tparams1), argtpes0, pt)
        case ErrorType =>
          true
        case _ =>
          false
      }

    /**
     * Todo: Try to make isApplicable always safe (i.e. not cause TypeErrors).
     * The chance of TypeErrors should be reduced through error trees
     */
    private[typechecker] def isApplicableSafe(undetparams: List[Symbol], ftpe: Type,
                                              argtpes0: List[Type], pt: Type): Boolean = {
      val reportAmbiguousErrors = context.reportAmbiguousErrors
      context.reportAmbiguousErrors = false
      try {
        isApplicable(undetparams, ftpe, argtpes0, pt)
      } catch {
        case ex: TypeError =>
          try {
            isApplicable(undetparams, ftpe, argtpes0, WildcardType)
          } catch {
            case ex: TypeError =>
              false
          }
      } finally {
        context.reportAmbiguousErrors = reportAmbiguousErrors
      }
    }

    /** Is type <code>ftpe1</code> strictly more specific than type <code>ftpe2</code>
     *  when both are alternatives in an overloaded function?
     *  @see SLS (sec:overloading-resolution)
     *
     *  @param ftpe1 ...
     *  @param ftpe2 ...
     *  @return      ...
     */
    def isAsSpecific(ftpe1: Type, ftpe2: Type): Boolean = ftpe1 match {
      case OverloadedType(pre, alts) =>
        alts exists (alt => isAsSpecific(pre.memberType(alt), ftpe2))
      case et: ExistentialType =>
        isAsSpecific(ftpe1.skolemizeExistential, ftpe2)
        //et.withTypeVars(isAsSpecific(_, ftpe2)) 
      case NullaryMethodType(res) =>
        isAsSpecific(res, ftpe2)
      case mt: MethodType if mt.isImplicit =>
        isAsSpecific(ftpe1.resultType, ftpe2)
      case MethodType(params, _) if params nonEmpty =>
        var argtpes = params map (_.tpe)
        if (isVarArgsList(params) && isVarArgsList(ftpe2.params))
          argtpes = argtpes map (argtpe => 
            if (isRepeatedParamType(argtpe)) argtpe.typeArgs.head else argtpe)
        isApplicable(List(), ftpe2, argtpes, WildcardType)
      case PolyType(tparams, NullaryMethodType(res)) =>
        isAsSpecific(PolyType(tparams, res), ftpe2)
      case PolyType(tparams, mt: MethodType) if mt.isImplicit =>
        isAsSpecific(PolyType(tparams, mt.resultType), ftpe2)
      case PolyType(_, MethodType(params, _)) if params nonEmpty =>
        isApplicable(List(), ftpe2, params map (_.tpe), WildcardType)
      // case NullaryMethodType(res) =>
      //   isAsSpecific(res, ftpe2)
      case ErrorType =>
        true
      case _ =>
        ftpe2 match {
          case OverloadedType(pre, alts) =>
            alts forall (alt => isAsSpecific(ftpe1, pre.memberType(alt)))
          case et: ExistentialType =>
            et.withTypeVars(isAsSpecific(ftpe1, _))
          case mt: MethodType =>
            !mt.isImplicit || isAsSpecific(ftpe1, mt.resultType)
          case NullaryMethodType(res) =>
            isAsSpecific(ftpe1, res)
          case PolyType(tparams, NullaryMethodType(res)) =>
            isAsSpecific(ftpe1, PolyType(tparams, res))
          case PolyType(tparams, mt: MethodType) =>
            !mt.isImplicit || isAsSpecific(ftpe1, PolyType(tparams, mt.resultType))
          case _ =>
            isAsSpecificValueType(ftpe1, ftpe2, List(), List())
        }
    }
    private def isAsSpecificValueType(tpe1: Type, tpe2: Type, undef1: List[Symbol], undef2: List[Symbol]): Boolean = (tpe1, tpe2) match {
      case (PolyType(tparams1, rtpe1), _) =>
        isAsSpecificValueType(rtpe1, tpe2, undef1 ::: tparams1, undef2)
      case (_, PolyType(tparams2, rtpe2)) =>
        isAsSpecificValueType(tpe1, rtpe2, undef1, undef2 ::: tparams2)
      case _ =>
        existentialAbstraction(undef1, tpe1) <:< existentialAbstraction(undef2, tpe2)
    }


/*
    def isStrictlyMoreSpecific(ftpe1: Type, ftpe2: Type): Boolean =
      ftpe1.isError || isAsSpecific(ftpe1, ftpe2) && 
      (!isAsSpecific(ftpe2, ftpe1) || 
       !ftpe1.isInstanceOf[OverloadedType] && ftpe2.isInstanceOf[OverloadedType] ||
       phase.erasedTypes && covariantReturnOverride(ftpe1, ftpe2))
*/
    /** Is sym1 (or its companion class in case it is a module) a subclass of
     *  sym2 (or its companion class in case it is a module)?
     */
    def isProperSubClassOrObject(sym1: Symbol, sym2: Symbol): Boolean =
      sym1 != sym2 && sym1 != NoSymbol && (sym1 isSubClass sym2) ||
      sym1.isModuleClass && isProperSubClassOrObject(sym1.linkedClassOfClass, sym2) ||
      sym2.isModuleClass && isProperSubClassOrObject(sym1, sym2.linkedClassOfClass)

    /** is symbol `sym1` defined in a proper subclass of symbol `sym2`?
     */
    def isInProperSubClassOrObject(sym1: Symbol, sym2: Symbol) = 
      sym2 == NoSymbol || isProperSubClassOrObject(sym1.owner, sym2.owner)

    def isStrictlyMoreSpecific(ftpe1: Type, ftpe2: Type, sym1: Symbol, sym2: Symbol): Boolean = {
      // ftpe1 / ftpe2 are OverloadedTypes (possibly with one single alternative) if they
      // denote the type of an "apply" member method (see "followApply")
      ftpe1.isError || {
        val specificCount = (if (isAsSpecific(ftpe1, ftpe2)) 1 else 0) - 
                            (if (isAsSpecific(ftpe2, ftpe1) &&
                                 // todo: move to isAsSpecific test
//                                 (!ftpe2.isInstanceOf[OverloadedType] || ftpe1.isInstanceOf[OverloadedType]) &&
                                 (!phase.erasedTypes || covariantReturnOverride(ftpe1, ftpe2))) 1 else 0)
        val subClassCount = (if (isInProperSubClassOrObject(sym1, sym2)) 1 else 0) -
                            (if (isInProperSubClassOrObject(sym2, sym1)) 1 else 0)
//        println("is more specific? "+sym1+":"+ftpe1+sym1.locationString+"/"+sym2+":"+ftpe2+sym2.locationString+":"+
//                specificCount+"/"+subClassCount)
        specificCount + subClassCount > 0
      }
    }
/*
      ftpe1.isError || {
        if (isAsSpecific(ftpe1, ftpe2)) 
          (!isAsSpecific(ftpe2, ftpe1) || 
           isProperSubClassOrObject(sym1.owner, sym2.owner) ||
           !ftpe1.isInstanceOf[OverloadedType] && ftpe2.isInstanceOf[OverloadedType] ||
           phase.erasedTypes && covariantReturnOverride(ftpe1, ftpe2))
        else
          !isAsSpecific(ftpe2, ftpe1) && 
          isProperSubClassOrObject(sym1.owner, sym2.owner)
      }
*/
    private def covariantReturnOverride(ftpe1: Type, ftpe2: Type): Boolean = (ftpe1, ftpe2) match {
      case (MethodType(_, rtpe1), MethodType(_, rtpe2)) =>
        rtpe1 <:< rtpe2 || rtpe2.typeSymbol == ObjectClass
      case _ =>
        false
    }
/*
    /** Is type `tpe1` a strictly better expression alternative than type `tpe2`?
     */
    def isStrictlyBetterExpr(tpe1: Type, tpe2: Type) = {
      isMethod(tpe2) && !isMethod(tpe1) ||
      isNullary(tpe1) && !isNullary(tpe2) ||
      isStrictlyBetter(tpe1, tpe2)
    }

    /** Is type `tpe1` a strictly better alternative than type `tpe2`?
     *  non-methods are always strictly better than methods
     *  nullary methods are always strictly better than non-nullary
     *  if both are non-nullary methods, then tpe1 is strictly better than tpe2 if
     *   - tpe1 specializes tpe2 and tpe2 does not specialize tpe1
     *   - tpe1 and tpe2 specialize each other and tpe1 has a strictly better resulttype than
     *     tpe2
     */
    def isStrictlyBetter(tpe1: Type, tpe2: Type) = {
      def isNullary(tpe: Type): Boolean = tpe match {
        case tp: RewrappingTypeProxy => isNullary(tp.underlying)
        case _ => tpe.paramSectionCount == 0 || tpe.params.isEmpty
      }
      def isMethod(tpe: Type): Boolean = tpe match {
        case tp: RewrappingTypeProxy => isMethod(tp.underlying)
        case MethodType(_, _) | PolyType(_, _) => true
        case _ => false
      }
      def hasStrictlyBetterResult = 
        resultIsBetter(tpe1, tpe2, List(), List()) && !resultIsBetter(tpe2, tpe1, List(), List())
      if (!isMethod(tpe1))
        isMethod(tpe2) || hasStrictlyBetterResult
      
      isNullary(tpe1) && !isNullary(tpe2) ||
      is
      
      else if (isNullary(tpe1))
        isMethod(tpe2) && (!isNullary(tpe2) || hasStrictlyBetterResult)
      else 
        specializes(tpe1, tpe2) && (!specializes(tpe2, tpe1) || hasStrictlyBetterResult)
    }

*/
    /** error if arguments not within bounds. */
    def checkBounds(pos: Position, pre: Type, owner: Symbol, 
                    tparams: List[Symbol], targs: List[Type], prefix: String): Option[ErrorTree] = {
      //@M validate variances & bounds of targs wrt variances & bounds of tparams
      //@M TODO: better place to check this? 
      //@M TODO: errors for getters & setters are reported separately
      val kindErrors = checkKindBounds(tparams, targs, pre, owner)
           
      if(!kindErrors.isEmpty) {
        if (targs contains WildcardType) None
        else Some(KindBoundErrors(pos, prefix, targs, tparams, kindErrors))
      } else if (!isWithinBounds(pre, owner, tparams, targs)) { 
        if (!(targs exists (_.isErroneous)) && !(tparams exists (_.isErroneous)))
          Some(NotWithinBounds(pos, prefix, targs, tparams, kindErrors))
        else None
      } else None
    }
    

    def checkKindBounds(tparams: List[Symbol], targs: List[Type], pre: Type, owner: Symbol): List[String] = {
      // @M TODO this method is duplicated all over the place (varianceString)
      def varStr(s: Symbol): String =
        if (s.isCovariant) "covariant"
        else if (s.isContravariant) "contravariant"
        else "invariant";                                                

      def qualify(a0: Symbol, b0: Symbol): String = if (a0.toString != b0.toString) "" else { 
        if((a0 eq b0) || (a0.owner eq b0.owner)) "" 
        else {
          var a = a0; var b = b0
          while (a.owner.name == b.owner.name) { a = a.owner; b = b.owner}
          if (a.locationString ne "") " (" + a.locationString.trim + ")" else ""
        }
      }

      val errors = checkKindBounds0(tparams, targs, pre, owner, true)
      val errorMessages = new ListBuffer[String]
      errors foreach {case (targ, tparam, arityMismatches, varianceMismatches, stricterBounds) => errorMessages +=
        (targ+"'s type parameters do not match "+tparam+"'s expected parameters: "+ 
        (for ((a, p) <- arityMismatches)
          yield a+qualify(a,p)+ " has "+countElementsAsString(a.typeParams.length, "type parameter")+", but "+
            p+qualify(p,a)+" has "+countAsString(p.typeParams.length)).toList.mkString(", ") +
        (for ((a, p) <- varianceMismatches)
          yield a+qualify(a,p)+ " is "+varStr(a)+", but "+
            p+qualify(p,a)+" is declared "+varStr(p)).toList.mkString(", ") +
        (for ((a, p) <- stricterBounds)
          yield a+qualify(a,p)+"'s bounds "+a.info+" are stricter than "+
            p+qualify(p,a)+"'s declared bounds "+p.info).toList.mkString(", "))
      }
      errorMessages.toList
    }
    /** Substitute free type variables `undetparams` of polymorphic argument
     *  expression `tree`, given two prototypes `strictPt`, and `lenientPt`.
     *  `strictPt` is the first attempt prototype where type parameters
     *  are left unchanged. `lenientPt` is the fall-back prototype where type
     *  parameters are replaced by `WildcardType`s. We try to instantiate
     *  first to `strictPt` and then, if this fails, to `lenientPt`. If both
     *  attempts fail, an error is produced.
     */
    def inferArgumentInstance(tree: Tree, undetparams: List[Symbol], strictPt: Type, lenientPt: Type) = {
      printInference(
        ptBlock("inferArgumentInstance",
          "tree"        -> tree,
          "tree.tpe"    -> tree.tpe,
          "undetparams" -> undetparams,
          "strictPt"    -> strictPt,
          "lenientPt"   -> lenientPt
        )
      )
      var targs = exprTypeArgs(undetparams, tree.tpe, strictPt)
      if ((targs eq null) || !(tree.tpe.subst(undetparams, targs) <:< strictPt)) {
        targs = exprTypeArgs(undetparams, tree.tpe, lenientPt)
      }
      printInference("[inferArgumentInstance] finished, targs = " + targs)
      substExpr(tree, undetparams, targs, lenientPt)
    }

    /** Infer type arguments `targs` for `tparams` of polymorphic expression in `tree`, given prototype `pt`.
     *
     * Substitute `tparams` to `targs` in `tree`, after adjustment by `adjustTypeArgs`, returning the type parameters that were not determined
     * If passed, infers against specified type `treeTp` instead of `tree.tp`.
     */
    def inferExprInstance(tree: Tree, tparams: List[Symbol], pt: Type = WildcardType, treeTp0: Type = null, keepNothings: Boolean = true, useWeaklyCompatible: Boolean = false): (Option[ErrorTree], List[Symbol]) = {
      val treeTp = if(treeTp0 eq null) tree.tpe else treeTp0 // can't refer to tree in default for treeTp0
      printInference(
        ptBlock("inferExprInstance",
          "tree"    -> tree,
          "tree.tpe"-> tree.tpe,
          "tparams" -> tparams,
          "pt"      -> pt
        )
      )
      val targs = exprTypeArgs(tparams, treeTp, pt, useWeaklyCompatible)

      if (keepNothings || (targs eq null)) { //@M: adjustTypeArgs fails if targs==null, neg/t0226
        (substExpr(tree, tparams, targs, pt), List())
      } else {
        val AdjustedTypeArgs.Undets(okParams, okArgs, leftUndet) = adjustTypeArgs(tparams, targs)
        printInference(
          ptBlock("inferExprInstance/AdjustedTypeArgs",
            "okParams" -> okParams,
            "okArgs" -> okArgs,
            "leftUndet" -> leftUndet
          )
        )
        (substExpr(tree, okParams, okArgs, pt), leftUndet)
      }
    }

    /** Substitute free type variables `undetparams` of polymorphic argument
     *  expression `tree` to `targs`, Error if `targs` is null.
     *
     *  @param tree ...
     *  @param undetparams ...
     *  @param targs ...
     *  @param pt ...
     */
    private def substExpr(tree: Tree, undetparams: List[Symbol],
                          targs: List[Type], pt: Type): Option[ErrorTree] = {
      if (targs eq null) {
        if (!tree.tpe.isErroneous && !pt.isErroneous)
          Some(PolymorphicExpressionInstantiationError(tree, undetparams, pt))
        else
          None
      } else {
        new TreeTypeSubstituter(undetparams, targs).traverse(tree)
        None
      }
    }

    /** Substitute free type variables <code>undetparams</code> of application
     *  <code>fn(args)</code>, given prototype <code>pt</code>.
     *
     *  @param fn          fn: the function that needs to be instantiated.
     *  @param undetparams the parameters that need to be determined
     *  @param args        the actual arguments supplied in the call.
     *  @param pt          the expected type of the function application
     *  @return            The type parameters that remain uninstantiated, 
     *                     and that thus have not been substituted.
     */
    def inferMethodInstance(fn: Tree, undetparams: List[Symbol],
                            args: List[Tree], pt0: Type): Either[ErrorTree, List[Symbol]] = fn.tpe match {
      case MethodType(params0, _) =>
        printInference(
          ptBlock("inferMethodInstance",
            "fn"          -> fn,
            "undetparams" -> undetparams,
            "args"        -> args,
            "pt0"         -> pt0
          )
        )

        try {
          val pt      = if (pt0.typeSymbol == UnitClass) WildcardType else pt0
          val formals = formalTypes(params0 map (_.tpe), args.length)
          val argtpes = actualTypes(args map (x => elimAnonymousClass(x.tpe.deconst)), formals.length)
          val restpe  = fn.tpe.resultType(argtpes)
          
          val AdjustedTypeArgs.AllArgsAndUndets(okparams, okargs, allargs, leftUndet) =
            methTypeArgs(undetparams, formals, restpe, argtpes, pt)
            
          checkBounds(fn.pos, NoPrefix, NoSymbol, undetparams, allargs, "inferred ") match {
            case Some(err) =>
              Left(err)
            case _ =>
              // bounds are ok
              val treeSubst = new TreeTypeSubstituter(okparams, okargs)
              treeSubst traverseTrees fn :: args
    
              val result = leftUndet match {
                case Nil  => Nil
                case xs   =>
                  // #3890
                  val xs1 = treeSubst.typeSubst mapOver xs
                  if (xs ne xs1)
                    new TreeSymSubstTraverser(xs, xs1) traverseTrees fn :: args
                  
                  xs1
              }
              if (result.nonEmpty)
                printInference("inferMethodInstance, still undetermined: " + result)
                
              Right(result)
          }
        }
        catch ifNoInstance { msg =>
          Left(NoMethodInstanceError(fn, args, msg))
        }
    }

    /** Type with all top-level occurrences of abstract types replaced by their bounds */
    def widen(tp: Type): Type = tp match { // @M don't normalize here (compiler loops on pos/bug1090.scala )
      case TypeRef(_, sym, _) if sym.isAbstractType => 
        widen(tp.bounds.hi)
      case TypeRef(_, sym, _) if sym.isAliasType => 
        widen(tp.normalize)
      case rtp @ RefinedType(parents, decls) => 
        copyRefinedType(rtp, parents mapConserve widen, decls)
      case AnnotatedType(_, underlying, _) =>
        widen(underlying)
      case _ =>
        tp
    }

    /** Substitute free type variables <code>undetparams</code> of type constructor
     *  <code>tree</code> in pattern, given prototype <code>pt</code>.
     *
     *  @param tree        the constuctor that needs to be instantiated
     *  @param undetparams the undetermined type parameters
     *  @param pt          the expected result type of the instance
     */
    def inferConstructorInstance(tree: Tree, undetparams: List[Symbol], pt0: Type): Option[ErrorTree] = {
      val pt = widen(pt0)
      //println("infer constr inst "+tree+"/"+undetparams+"/"+pt0)
      var restpe = tree.tpe.finalResultType
      var tvars = undetparams map freshVar

      /** Compute type arguments for undetermined params and substitute them in given tree.
       */
      def computeArgs =
        try {
          val targs = solvedTypes(tvars, undetparams, undetparams map varianceInType(restpe), 
                                  true, lubDepth(List(restpe, pt)))
//          checkBounds(tree.pos, NoPrefix, NoSymbol, undetparams, targs, "inferred ")
//          no checkBounds here. If we enable it, test bug602 fails.
          new TreeTypeSubstituter(undetparams, targs).traverse(tree)
          None
        } catch ifNoInstance{ msg =>
          Some(NoConstructorInstanceError(tree, restpe, pt, msg))
        }
      def instError = {
        if (settings.debug.value) Console.println("ici " + tree + " " + undetparams + " " + pt)
        if (settings.explaintypes.value) explainTypes(restpe.instantiateTypeParams(undetparams, tvars), pt)
        Some(ConstrInstantiationError(tree, restpe, pt))
      }
      if (restpe.instantiateTypeParams(undetparams, tvars) <:< pt) {
        computeArgs
      } else if (isFullyDefined(pt)) {
        debuglog("infer constr " + tree + ":" + restpe + ", pt = " + pt)
        var ptparams = freeTypeParamsOfTerms.collect(pt)
        debuglog("free type params = " + ptparams)
        val ptWithWildcards = pt.instantiateTypeParams(ptparams, ptparams map (ptparam => WildcardType))
        tvars = undetparams map freshVar
        if (restpe.instantiateTypeParams(undetparams, tvars) <:< ptWithWildcards) {
          computeArgs
          restpe = skipImplicit(tree.tpe.resultType)
          debuglog("new tree = " + tree + ":" + restpe)
          val ptvars = ptparams map freshVar
          val pt1 = pt.instantiateTypeParams(ptparams, ptvars)
          if (isPopulated(restpe, pt1)) {
            ptvars foreach instantiateTypeVar
            None
          } else { if (settings.debug.value) Console.println("no instance: "); instError }
        } else { if (settings.debug.value) Console.println("not a subtype " + restpe.instantiateTypeParams(undetparams, tvars) + " of " + ptWithWildcards); instError }
      } else { if (settings.debug.value) Console.println("not fully defined: " + pt); instError }
    }

    def instBounds(tvar: TypeVar): (Type, Type) = {
      val tparam = tvar.origin.typeSymbol
      val instType = toOrigin(tvar.constr.inst)
      val (loBounds, hiBounds) =
        if (instType != NoType && isFullyDefined(instType)) (List(instType), List(instType))
        else (tvar.constr.loBounds, tvar.constr.hiBounds)
      val lo = lub(tparam.info.bounds.lo :: loBounds map toOrigin)
      val hi = glb(tparam.info.bounds.hi :: hiBounds map toOrigin)
      (lo, hi)
    }

    def isInstantiatable(tvars: List[TypeVar]) = {
      val tvars1 = tvars map (_.cloneInternal)
      // Note: right now it's not clear that solving is complete, or how it can be made complete!
      // So we should come back to this and investigate.
      solve(tvars1, tvars1 map (_.origin.typeSymbol), tvars1 map (x => COVARIANT), false)  
    }

    // this is quite nasty: it destructively changes the info of the syms of e.g., method type params (see #3692, where the type param T's bounds were set to >: T <: T, so that parts looped)
    // the changes are rolled back by restoreTypeBounds, but might be unintentially observed in the mean time
    def instantiateTypeVar(tvar: TypeVar) {
      val tparam = tvar.origin.typeSymbol
      if (false && 
          tvar.constr.inst != NoType && 
          isFullyDefined(tvar.constr.inst) && 
          (tparam.info.bounds containsType tvar.constr.inst)) {
        context.nextEnclosing(_.tree.isInstanceOf[CaseDef]).pushTypeBounds(tparam)
        tparam setInfo tvar.constr.inst
        tparam resetFlag DEFERRED
        debuglog("new alias of " + tparam + " = " + tparam.info)
      } else {
        val (lo, hi) = instBounds(tvar)
        if (lo <:< hi) {
          if (!((lo <:< tparam.info.bounds.lo) && (tparam.info.bounds.hi <:< hi)) // bounds were improved
             && tparam != lo.typeSymbolDirect && tparam != hi.typeSymbolDirect) { // don't create illegal cycles
            context.nextEnclosing(_.tree.isInstanceOf[CaseDef]).pushTypeBounds(tparam)
            tparam setInfo TypeBounds(lo, hi)
            debuglog("new bounds of " + tparam + " = " + tparam.info)
          } else {
            debuglog("redundant: "+tparam+" "+tparam.info+"/"+lo+" "+hi)
          }
        } else {
          debuglog("inconsistent: "+tparam+" "+lo+" "+hi)
        }
      }
    }

    def checkCheckable(pos: Position, tp: Type, kind: String): Option[ErrorTree] = {
      def patternWarning(tp0: Type, prefix: String) = {
        context.unit.uncheckedWarning(pos, prefix+tp0+" in type "+kind+tp+" is unchecked since it is eliminated by erasure")
      }
      def check(tp: Type, bound: List[Symbol]): Option[ErrorTree] = {
        implicit def listErrorsToPending(l: List[ErrorTree]): Option[ErrorTree] = if (l.isEmpty) None else Some(PendingErrors(l))
        def isLocalBinding(sym: Symbol) =
          sym.isAbstractType && 
          ((bound contains sym) ||
           sym.name == tpnme.WILDCARD || {
            val e = context.scope.lookupEntry(sym.name)
            (e ne null) && e.sym == sym && !e.sym.isTypeParameterOrSkolem && e.owner == context.scope
          })
        tp match {
          case SingleType(pre, _) => 
            check(pre, bound)
          case TypeRef(pre, sym, args) => 
            val checkForSymAndArgs: Option[ErrorTree] = if (sym.isAbstractType) {
              if (!isLocalBinding(sym)) patternWarning(tp, "abstract type ")
              None
            } else if (sym.isAliasType) {
              check(tp.normalize, bound)
            } else if (sym == NothingClass || sym == NullClass || sym == AnyValClass) {
              Some(TypePatternOrIsInstanceTestError(pos, tp))
            } else {
              args.map( arg => {
                if (sym == ArrayClass) check(arg, bound)
                else if (arg.typeArgs.nonEmpty) None   // avoid spurious warnings with higher-kinded types
                else {arg match {
                  case TypeRef(_, sym, _) if isLocalBinding(sym) =>
                    
                  case _ =>
                    patternWarning(arg, "non variable type-argument ")
                }; None}
              }).flatten
            }
            List(checkForSymAndArgs, check(pre, bound)).flatten
          case RefinedType(parents, decls) =>
            if (decls.isEmpty) parents.map(p => check(p, bound)).flatten
            else { patternWarning(tp, "refinement "); None }
          case ExistentialType(quantified, tp1) =>
            check(tp1, bound ::: quantified)
          case ThisType(_) =>
            None
          case NoPrefix => 
            None
          case _ => 
            patternWarning(tp, "type ")
            None
        }
      }
      check(tp, List())
    }
     
    /** Type intersection of simple type tp1 with general type tp2.
     *  The result eliminates some redundancies.
     */
    def intersect(tp1: Type, tp2: Type): Type = {
      if (tp1 <:< tp2) tp1
      else if (tp2 <:< tp1) tp2
      else {
        val reduced2 = tp2 match {
          case rtp @ RefinedType(parents2, decls2) =>
            copyRefinedType(rtp, parents2 filterNot (tp1 <:< _), decls2)
          case _ =>
            tp2
        }
        intersectionType(List(tp1, reduced2))
      }
    }

    def inferTypedPattern(pos: Position, pattp: Type, pt0: Type): Either[ErrorTree, Type] = {
      val pt        = widen(pt0)
      val ptparams  = freeTypeParamsOfTerms.collect(pt)
      val tpparams  = freeTypeParamsOfTerms.collect(pattp)

      def ptMatchesPattp = pt matchesPattern pattp.widen
      def pattpMatchesPt = pattp matchesPattern pt
      
      /** If we can absolutely rule out a match we can fail early.
       *  This is the case if the scrutinee has no unresolved type arguments
       *  and is a "final type", meaning final + invariant in all type parameters.
       */
      if (pt.isFinalType && ptparams.isEmpty && !ptMatchesPattp)
        return Left(IncompatibleScrutineeTypeError(pos, pattp, pt))
      
      checkCheckable(pos, pattp, "pattern ") match {
        case Some(err) =>
          return Left(err)
        case _ => ()
      }
      if (pattp <:< pt) ()
      else {
        debuglog("free type params (1) = " + tpparams)
          
        var tvars = tpparams map freshVar
        var tp    = pattp.instantiateTypeParams(tpparams, tvars)
        
        if ((tp <:< pt) && isInstantiatable(tvars)) ()
        else {
          tvars = tpparams map freshVar
          tp    = pattp.instantiateTypeParams(tpparams, tvars)

          debuglog("free type params (2) = " + ptparams)
          
          val ptvars = ptparams map freshVar
          val pt1    = pt.instantiateTypeParams(ptparams, ptvars)  
          
          // See ticket #2486 for an example of code which would incorrectly
          // fail if we didn't allow for pattpMatchesPt.
          if (isPopulated(tp, pt1) && isInstantiatable(tvars ++ ptvars) || pattpMatchesPt)
             ptvars foreach instantiateTypeVar
          else
            return Left(IncompletePatternTypeError(pos, pattp, pt))            
        }
        tvars foreach instantiateTypeVar
      }
      /** If the scrutinee has free type parameters but the pattern does not,
       *  we have to flip the arguments so the expected type is treated as more
       *  general when calculating the intersection.  See run/bug2755.scala.
       */
      if (tpparams.isEmpty && ptparams.nonEmpty) Right(intersect(pattp, pt))
      else Right(intersect(pt, pattp))
    }

    def inferModulePattern(pat: Tree, pt: Type): Option[ErrorTree] =
      if (!(pat.tpe <:< pt)) {
        val ptparams = freeTypeParamsOfTerms.collect(pt)
        debuglog("free type params (2) = " + ptparams)
        val ptvars = ptparams map freshVar
        val pt1 = pt.instantiateTypeParams(ptparams, ptvars)
        if (pat.tpe <:< pt1) {
          ptvars foreach instantiateTypeVar
          None
        } else
          Some(PatternTypeIncompatibleWithPtError(pat, pt1, pt))
      } else None

    object toOrigin extends TypeMap {
      def apply(tp: Type): Type = tp match {
        case TypeVar(origin, _) => origin
        case _ => mapOver(tp)
      }
    }

    abstract class SymCollector extends TypeCollector(List[Symbol]()) {
      protected def includeCondition(sym: Symbol): Boolean

      def traverse(tp: Type) {
        tp.normalize match {
          case TypeRef(_, sym, _) =>
            if (includeCondition(sym) && !result.contains(sym)) result = sym :: result
          case _ =>
        }
        mapOver(tp)
      }
    }

    object approximateAbstracts extends TypeMap {
      def apply(tp: Type): Type = tp.normalize match {
        case TypeRef(pre, sym, _) if sym.isAbstractType => WildcardType
        case _ => mapOver(tp)
      }
    }

    /** A traverser to collect type parameters referred to in a type
     */
    object freeTypeParamsOfTerms extends SymCollector {
      protected def includeCondition(sym: Symbol): Boolean =
        sym.isAbstractType && sym.owner.isTerm
    }

    /** A traverser to collect type parameters referred to in a type
     */
    object freeTypeParametersNoSkolems extends SymCollector {
      protected def includeCondition(sym: Symbol): Boolean =
        sym.isTypeParameter && sym.owner.isTerm
    }

    object typeRefs extends SymCollector {
      protected def includeCondition(sym: Symbol): Boolean = true
    }

    /* -- Overload Resolution ---------------------------------------------- */

/*
    def checkNotShadowed(pos: Position, pre: Type, best: Symbol, eligible: List[Symbol]) =
      if (!phase.erasedTypes)
        for (alt <- eligible) {
          if (isProperSubClassOrObject(alt.owner, best.owner))
            error(pos,
                  "erroneous reference to overloaded definition,\n"+
                  "most specific definition is: "+best+best.locationString+" of type "+pre.memberType(best)+
                  ",\nyet alternative definition   "+alt+alt.locationString+" of type "+pre.memberType(alt)+
                  "\nis defined in a subclass")
        }
*/

    /** Assign <code>tree</code> the symbol and type of the alternative which
     *  matches prototype <code>pt</code>, if it exists.
     *  If several alternatives match `pt`, take parameterless one.
     *  If no alternative matches `pt`, take the parameterless one anyway.
     */
    def inferExprAlternative(tree: Tree, pt: Type): Option[ErrorTree] = tree.tpe match {
      case OverloadedType(pre, alts) => tryTwice {
        val alts0     = alts filter (alt => isWeaklyCompatible(pre.memberType(alt), pt))
        val secondTry = alts0.isEmpty
        val alts1     = if (secondTry) alts else alts0

        //println("trying "+alts1+(alts1 map (_.tpe))+(alts1 map (_.locationString))+" for "+pt)
        def improves(sym1: Symbol, sym2: Symbol): Boolean =
          sym2 == NoSymbol || sym2.hasAnnotation(BridgeClass) ||
          { val tp1 = pre.memberType(sym1)
            val tp2 = pre.memberType(sym2)
            (tp2 == ErrorType ||
             !global.typer.infer.isWeaklyCompatible(tp2, pt) && global.typer.infer.isWeaklyCompatible(tp1, pt) ||
             isStrictlyMoreSpecific(tp1, tp2, sym1, sym2)) }

        val best = ((NoSymbol: Symbol) /: alts1) ((best, alt) =>
          if (improves(alt, best)) alt else best)
          
        val competing = alts1 dropWhile (alt => best == alt || improves(best, alt))
        
        if (best == NoSymbol) {
          if (settings.debug.value) {
            tree match {
              case Select(qual, _) =>
                Console.println("qual: " + qual + ":" + qual.tpe +
                                   " with decls " + qual.tpe.decls +
                                   " with members " + qual.tpe.members +
                                   " with members " + qual.tpe.member(newTermName("$minus")))
              case _ =>
            }
          }
          Some(NoBestExprAlternativeError(tree, pt))
        } else if (!competing.isEmpty) {
          if (secondTry) {
            Some(NoBestExprAlternativeError(tree, pt))
          } else {
            if (!pt.isErroneous)
              Some(AmbiguousExprAlternativeError(tree, pre, best, competing.head, pt))
            else
              Some(NullErrorTree) // already reported
          }
        } else {
//          val applicable = alts1 filter (alt => 
//            global.typer.infer.isWeaklyCompatible(pre.memberType(alt), pt))
//          checkNotShadowed(tree.pos, pre, best, applicable)
          tree.setSymbol(best).setType(pre.memberType(best))
          None
        }
      }
    }
    
    // TODO: remove once error tree refactoring is done
    @inline private def wrapTypeError(expr: => Boolean): Boolean =
      try expr
      catch { case _: TypeError => false }
    
    // Checks against the name of the parameter and also any @deprecatedName.
    private def paramMatchesName(param: Symbol, name: Name) =
      param.name == name || param.deprecatedParamName.exists(_ == name)
    
    // Check the first parameter list the same way.
    private def methodMatchesName(method: Symbol, name: Name) = method.paramss match {
      case ps :: _  => ps exists (p => paramMatchesName(p, name))
      case _        => false
    }

    private def resolveOverloadedMethod(argtpes: List[Type], eligible: List[Symbol]) = {
      // If there are any foo=bar style arguments, and any of the overloaded
      // methods has a parameter named `foo`, then only those methods are considered.
      val namesOfArgs = argtpes collect { case NamedType(name, _) => name }
      val namesMatch = (
        if (namesOfArgs.isEmpty) Nil
        else eligible filter { m =>
          namesOfArgs forall { name =>
            methodMatchesName(m, name)
          }
        }
      )
      
      if (namesMatch.nonEmpty) namesMatch
      else if (eligible.isEmpty || eligible.tail.isEmpty) eligible
      else eligible filter { alt =>
        // for functional values, the `apply` method might be overloaded
        val mtypes = followApply(alt.tpe) match {
          case OverloadedType(_, alts) => alts map (_.tpe)
          case t                       => List(t)
        }
        // Drop those that use a default; keep those that use vararg/tupling conversion.
        mtypes exists (t =>
          !t.typeSymbol.hasDefaultFlag && {
            compareLengths(t.params, argtpes) < 0 ||  // tupling (*)
            hasExactlyNumParams(t, argtpes.length)    // same nb or vararg
          }
        )
        // (*) more arguments than parameters, but still applicable: tupling conversion works.
        //     todo: should not return "false" when paramTypes = (Unit) no argument is given
        //     (tupling would work)
      }
    }

    /** Assign <code>tree</code> the type of an alternative which is applicable
     *  to <code>argtpes</code>, and whose result type is compatible with `pt`.
     *  If several applicable alternatives exist, drop the alternatives which use
     *  default arguments, then select the most specialized one.
     *  If no applicable alternative exists, and pt != WildcardType, try again
     *  with pt = WildcardType.
     *  Otherwise, if there is no best alternative, error.
     *
     *  @param argtpes contains the argument types. If an argument is named, as
     *    "a = 3", the corresponding type is `NamedType("a", Int)'. If the name
     *    of some NamedType does not exist in an alternative's parameter names,
     *    the type is replaces by `Unit`, i.e. the argument is treated as an
     *    assignment expression.
     */
    def inferMethodAlternative(tree: Tree, undetparams: List[Symbol],
                               argtpes: List[Type], pt0: Type, varArgsOnly: Boolean = false): Option[ErrorTree] = tree.tpe match {
      case OverloadedType(pre, alts) =>
        val pt = if (pt0.typeSymbol == UnitClass) WildcardType else pt0
        tryTwice {
          debuglog("infer method alt "+ tree.symbol +" with alternatives "+
                (alts map pre.memberType) +", argtpes = "+ argtpes +", pt = "+ pt)

          val applicable = resolveOverloadedMethod(argtpes, {
            alts filter { alt =>
              // TODO: this will need to be re-written once we substitute throwing exceptions
              // with generating error trees. We wrap this applicability in try/catch because of #4457.
              wrapTypeError(isApplicable(undetparams, followApply(pre.memberType(alt)), argtpes, pt)) &&
              (!varArgsOnly || isVarArgsList(alt.tpe.params))
            }
          })

          def improves(sym1: Symbol, sym2: Symbol) = {
            // util.trace("improve "+sym1+sym1.locationString+" on "+sym2+sym2.locationString)
            sym2 == NoSymbol || sym2.isError || sym2.hasAnnotation(BridgeClass) ||
            isStrictlyMoreSpecific(followApply(pre.memberType(sym1)),
                                   followApply(pre.memberType(sym2)), sym1, sym2)
          }

          val best = ((NoSymbol: Symbol) /: applicable) ((best, alt) =>
            if (improves(alt, best)) alt else best)
          val competing = applicable.dropWhile(alt => best == alt || improves(best, alt))
          if (best == NoSymbol) {
            if (pt == WildcardType)
              Some(NoBestMethodAlternativeError(tree, argtpes, pt))
            else
              inferMethodAlternative(tree, undetparams, argtpes, WildcardType)
          } else if (!competing.isEmpty) {
            if (!(argtpes exists (_.isErroneous)) && !pt.isErroneous)
              Some(AmbiguousMethodAlternativeError(tree, pre, best, competing.head, argtpes, pt))
            else
              Some(NullErrorTree)
          } else {
//            checkNotShadowed(tree.pos, pre, best, applicable)
            tree.setSymbol(best).setType(pre.memberType(best))
            None
          }
        }
      case _ => None
    }

    /** Try inference twice, once without views and once with views,
     *  unless views are already disabled.
     *
     *  @param infer ...
     */
    def tryTwice(infer: => Option[ErrorTree]): Option[ErrorTree] = {
      if (context.implicitsEnabled) {
        val reportGeneralErrors = context.reportGeneralErrors
        context.reportGeneralErrors = false
        val res = try {
          context.withImplicitsDisabled(infer) match {
            case Some(err) =>
              context.reportGeneralErrors = reportGeneralErrors
              infer
            case ok => ok
          }
        } catch {
          case ex: CyclicReference  => throw ex
          case ex: TypeError        =>
            context.reportGeneralErrors = reportGeneralErrors
            infer
        }
        context.reportGeneralErrors = reportGeneralErrors
        res
      }
      else infer
    }

    /** Assign <code>tree</code> the type of unique polymorphic alternative
     *  with <code>nparams</code> as the number of type parameters, if it exists.
     *  If several or none such polymorphic alternatives exist, error.
     *
     *  @param tree ...
     *  @param nparams ...
     */
    def inferPolyAlternatives(tree: Tree, argtypes: List[Type]): Option[ErrorTree] = {
      val OverloadedType(pre, alts) = tree.tpe
      val sym0 = tree.symbol filter (alt => sameLength(alt.typeParams, argtypes))
      def fail(msg: String) = Some(PolyAlternativeError(tree, msg))

      if (sym0 == NoSymbol) return fail(
        if (alts exists (_.typeParams.nonEmpty))
          "wrong number of type parameters for " + treeSymTypeMsg(tree)
        else treeSymTypeMsg(tree) + " does not take type parameters"
      )

      val (resSym, resTpe) = {
        if (!sym0.isOverloaded)
          (sym0, pre.memberType(sym0))
        else {
          val sym = sym0 filter (alt => isWithinBounds(pre, alt.owner, alt.typeParams, argtypes))
          if (sym == NoSymbol) {
            if (argtypes forall (x => !x.isErroneous))
              return fail(
              "type arguments " + argtypes.mkString("[", ",", "]") + 
              " conform to the bounds of none of the overloaded alternatives of\n "+sym0+
              ": "+sym0.info
            )
            return None
          }
          else if (sym.isOverloaded) {
            val xs      = sym.alternatives
            val tparams = new AsSeenFromMap(pre, xs.head.owner) mapOver xs.head.typeParams
            val bounds  = tparams map (_.tpeHK) // see e.g., #1236
            val tpe     = PolyType(tparams, OverloadedType(AntiPolyType(pre, bounds), xs))
            
            (sym setInfo tpe, tpe)
          }
          else (sym, pre.memberType(sym))
        }
      }
      // Side effects tree with symbol and type
      tree setSymbol resSym setType resTpe
      None
    }
  }
}

