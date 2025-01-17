/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer

abstract class Mixin extends InfoTransform with ast.TreeDSL {
  import global._
  import definitions._
  import CODE._

  /** The name of the phase: */
  val phaseName: String = "mixin"

  /** The phase might set the following new flags: */
  override def phaseNewFlags: Long = lateMODULE | notABSTRACT

  /** This map contains a binding (class -> info) if
   *  the class with this info at phase mixinPhase has been treated for mixin composition 
   */
  private val treatedClassInfos = perRunCaches.newMap[Symbol, Type]()

// --------- helper functions -----------------------------------------------

  /** A member of a trait is implemented statically if its implementation after the
   *  mixin transform is in the static implementation module. To be statically
   *  implemented, a member must be a method that belonged to the trait's implementation class
   *  before (e.g. it is not abstract). Not statically implemented are
   *   - non-private modules: these are implemented directly in the mixin composition class
   *     (private modules, on the other hand, are implemented statically, but their
   *      module variable is not. all such private modules are lifted, because 
   *      non-lifted private modules have been eliminated in ExplicitOuter)
   *   - field accessors and superaccessors, except for lazy value accessors which become initializer
   *     methods in the impl class (because they can have arbitrary initializers)
   */
  private def isImplementedStatically(sym: Symbol) =
    sym.owner.isImplClass && sym.isMethod &&
    (!sym.isModule || sym.hasFlag(PRIVATE | LIFTED)) &&
    (!(sym hasFlag (ACCESSOR | SUPERACCESSOR)) || sym.isLazy)

  /** A member of a trait is static only if it belongs only to the
   *  implementation class, not the interface, and it is implemented
   *  statically.
   */
  private def isStaticOnly(sym: Symbol) =
    isImplementedStatically(sym) && sym.isImplOnly

  /** A member of a trait is forwarded if it is implemented statically and it
   *  is also visible in the trait's interface. In that case, a forwarder to
   *  the member's static implementation will be added to the class that
   *  inherits the trait.
   */
  private def isForwarded(sym: Symbol) =
    isImplementedStatically(sym) && !sym.isImplOnly

  /** Maps the type of an implementation class to its interface;
   *  maps all other types to themselves.
   */
  private def toInterface(tp: Type): Type =
    atPhase(currentRun.mixinPhase)(tp.typeSymbol.toInterface).tpe

  /** Maps all parts of this type that refer to implementation classes to
   *  their corresponding interfaces.
   */
  private val toInterfaceMap = new TypeMap {
    def apply(tp: Type): Type = mapOver( tp match {
      case TypeRef(pre, sym, args) if (sym.isImplClass) =>
        typeRef(pre, atPhase(currentRun.mixinPhase)(sym.toInterface), args)
      case _ => tp
    })
  }

  /** The implementation class corresponding to a currently compiled interface.
   *  todo: try to use Symbol.implClass instead?
   */
  private def implClass(iface: Symbol): Symbol = {
    val impl = iface.implClass
    if (impl != NoSymbol) impl else erasure.implClass(iface)
  }

  /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
   *
   *  @param base       The class in which everything is mixed together
   *  @param member     The symbol statically referred to by the superaccessor in the trait
   *  @param mixinClass The mixin class that produced the superaccessor
   */
  private def rebindSuper(base: Symbol, member: Symbol, mixinClass: Symbol): Symbol =
    atPhase(currentRun.picklerPhase.next) {
      var bcs = base.info.baseClasses.dropWhile(mixinClass !=).tail
      var sym: Symbol = NoSymbol
      debuglog("starting rebindsuper " + base + " " + member + ":" + member.tpe +
            " " + mixinClass + " " + base.info.baseClasses + "/" + bcs)
      while (!bcs.isEmpty && sym == NoSymbol) {
        if (settings.debug.value) {
          val other = bcs.head.info.nonPrivateDecl(member.name);
          log("rebindsuper " + bcs.head + " " + other + " " + other.tpe +
              " " + other.isDeferred)
        }
        sym = member.matchingSymbol(bcs.head, base.thisType).suchThat(sym => !sym.hasFlag(DEFERRED | BRIDGE))
        bcs = bcs.tail
      }
      assert(sym != NoSymbol, member)
      sym
    }

// --------- type transformation -----------------------------------------------

  def isConcreteAccessor(member: Symbol) = 
    member.hasAccessorFlag && (!member.isDeferred || (member hasFlag lateDEFERRED))

  /** Is member overridden (either directly or via a bridge) in base class sequence `bcs`? */
  def isOverriddenAccessor(member: Symbol, bcs: List[Symbol]): Boolean = atPhase(ownPhase) {
    def hasOverridingAccessor(clazz: Symbol) = {
      clazz.info.nonPrivateDecl(member.name).alternatives.exists(
        sym => 
          isConcreteAccessor(sym) && 
          !sym.hasFlag(MIXEDIN) &&
          matchesType(sym.tpe, member.tpe, true))
    }
    bcs.head != member.owner && 
    (hasOverridingAccessor(bcs.head) || isOverriddenAccessor(member, bcs.tail))
  }

  /** Add given member to given class, and mark member as mixed-in.
   */
  def addMember(clazz: Symbol, member: Symbol): Symbol = {
    debuglog("new member of " + clazz + ":" + member.defString)
    clazz.info.decls enter member
    member.setFlag(MIXEDIN)
  }

  def needsExpandedSetterName(field: Symbol) = !field.isLazy && (
    if (field.isMethod) field.hasStableFlag
    else !field.isMutable
  )

  /** Add getters and setters for all non-module fields of an implementation
   *  class to its interface unless they are already present. This is done
   *  only once per class. The mixedin flag is used to remember whether late
   *  members have been added to an interface.
   *    - lazy fields don't get a setter.
   */
  def addLateInterfaceMembers(clazz: Symbol) {
    if ((treatedClassInfos get clazz) != Some(clazz.info)) {
      treatedClassInfos(clazz) = clazz.info
      assert(phase == currentRun.mixinPhase)

      /** Create a new getter. Getters are never private or local. They are
       *  always accessors and deferred. */
      def newGetter(field: Symbol): Symbol = {
        // println("creating new getter for "+ field +" : "+ field.info +" at "+ field.locationString+(field hasFlag MUTABLE))
        // atPhase(currentRun.erasurePhase){ 
        //   println("before erasure: "+ (field.info))
        // }
        clazz.newMethod(field.pos, nme.getterName(field.name))
          .setFlag(field.flags & ~(PRIVATE | LOCAL) | ACCESSOR | lateDEFERRED | 
                     (if (field.isMutable) 0 else STABLE))
          .setInfo(MethodType(List(), field.info)) // TODO preserve pre-erasure info?
      }

      /** Create a new setter. Setters are never private or local. They are
       *  always accessors and deferred. */
      def newSetter(field: Symbol): Symbol = {
        //println("creating new setter for "+field+field.locationString+(field hasFlag MUTABLE))
        val setterName = nme.getterToSetter(nme.getterName(field.name))
        val setter = clazz.newMethod(field.pos, setterName)
          .setFlag(field.flags & ~(PRIVATE | LOCAL) | ACCESSOR | lateDEFERRED)
        setter.setInfo(MethodType(setter.newSyntheticValueParams(List(field.info)), UnitClass.tpe))  // TODO preserve pre-erasure info?
        if (needsExpandedSetterName(field)) {
          //println("creating expanded setter from "+field)
          setter.name = nme.expandedSetterName(setter.name, clazz)
        }
        setter
      }

      clazz.info // make sure info is up to date, so that implClass is set.
      val impl = implClass(clazz)
      assert(impl != NoSymbol)

      for (member <- impl.info.decls) {
        if (!member.isMethod && !member.isModule && !member.isModuleVar) {
          assert(member.isTerm && !member.isDeferred, member)
          if (member.getter(impl).isPrivate) {
            member.makeNotPrivate(clazz) // this will also make getter&setter not private
          }
          val getter = member.getter(clazz)
          if (getter == NoSymbol) addMember(clazz, newGetter(member))
          if (!member.tpe.isInstanceOf[ConstantType] && !member.isLazy) {
            val setter = member.setter(clazz, needsExpandedSetterName(member))
            if (setter == NoSymbol) addMember(clazz, newSetter(member))
          }
        }
      }
      debuglog("new defs of " + clazz + " = " + clazz.info.decls);
    }
  }

  /** Map a lazy, mixedin field accessor to it's trait member accessor */
  val initializer = perRunCaches.newMap[Symbol, Symbol]
  
  /** Deferred bitmaps that will be added during the transformation of a class */
  val deferredBitmaps = perRunCaches.newMap[Symbol, List[Tree]]()
  
  /** Add all members to be mixed in into a (non-trait-) class
   *  These are:
   *    for every mixin trait T that is not also inherited by the superclass:
   *     add late interface members to T and then:
   *      - if a member M of T is forwarded to the implementation class, add
   *        a forwarder for M unless one exists already.
   *        The alias of the forwarder is the static member it forwards to.
   *      - for every abstract accessor in T, add a field and an implementation for that accessor
   *      - for every super accessor in T, add an implementation of that accessor
   *      - for every module in T, add a module
   */
  def addMixedinMembers(clazz: Symbol, unit : CompilationUnit) {
    def cloneBeforeErasure(iface: Symbol, clazz: Symbol, imember: Symbol): Symbol = {
      val newSym = atPhase(currentRun.erasurePhase){
        val res = imember.cloneSymbol(clazz)
        // since we used the member (imember) from the interface that represents the trait that's being mixed in, 
        // have to instantiate the interface type params (that may occur in imember's info) as they are seen from the class
        // we can't use the member that we get from the implementation class, as it's a clone that was made after erasure, 
        // and thus it does not know its info at the beginning of erasure anymore
        // optimize: no need if iface has no typeparams
        if(iface.typeParams nonEmpty) res.setInfo(clazz.thisType.baseType(iface).memberInfo(imember)) 
        res
      } // clone before erasure got rid of type info we'll need to generate a javaSig
      // now we'll have the type info at (the beginning of) erasure in our history,
      newSym.updateInfo(imember.info.cloneInfo(newSym)) // and now newSym has the info that's been transformed to fit this period (no need for asSeenFrom as phase.erasedTypes)
      newSym // TODO: verify we need the updateInfo and document why
    }

    if (!(clazz hasFlag JAVA) && (treatedClassInfos get clazz) != Some(clazz.info)) {
      treatedClassInfos(clazz) = clazz.info

      assert(!clazz.isTrait, clazz)
      assert(clazz.info.parents.nonEmpty, clazz)

      // first complete the superclass with mixed in members
      addMixedinMembers(clazz.superClass, unit)

      //Console.println("adding members of " + clazz.info.baseClasses.tail.takeWhile(superclazz !=) + " to " + clazz);//DEBUG

      /** Mix in members of implementation class mixinClass into class clazz */
      def mixinImplClassMembers(impl: Symbol, iface: Symbol) {
        assert(
          // XXX this should be impl.isImplClass, except that we get impl classes
          // coming through under -optimise which do not agree that they are (because
          // the IMPLCLASS flag is unset, I believe.) See ticket #4285.
          nme.isImplClassName(impl.name) || impl.isImplClass,
          "%s (%s) is not a an implementation class, it cannot mix in %s".format(
            impl, impl.defaultFlagString, iface)
        )
        for (member <- impl.info.decls) {
          if (isForwarded(member)) {
            val imember = member.overriddenSymbol(iface)
            // atPhase(currentRun.erasurePhase){ 
            //   println(""+(clazz, iface, clazz.typeParams, iface.typeParams, imember, clazz.thisType.baseType(iface), clazz.thisType.baseType(iface).memberInfo(imember), imember.info substSym(iface.typeParams, clazz.typeParams)  ))
            // }
            // Console.println("mixin member "+member+":"+member.tpe+member.locationString+" "+imember+" "+imember.overridingSymbol(clazz)+" to "+clazz+" with scope "+clazz.info.decls)//DEBUG
            if (imember.overridingSymbol(clazz) == NoSymbol &&
                clazz.info.findMember(member.name, 0, lateDEFERRED, false).alternatives.contains(imember)) {
                  val member1 = addMember(
                    clazz,
                    cloneBeforeErasure(iface, clazz, imember) setPos clazz.pos resetFlag (DEFERRED | lateDEFERRED))
                  member1.asInstanceOf[TermSymbol] setAlias member;
                }
          }
        }
      }

      /** Mix in members of trait mixinClass into class clazz. Also,
       *  for each lazy field in mixinClass, add a link from its mixed in member to its
       *  initializer method inside the implclass.
       */
      def mixinTraitMembers(mixinClass: Symbol) {
        // For all members of a trait's interface do:
        for (member <- mixinClass.info.decls) {
          if (isConcreteAccessor(member)) {
            if (isOverriddenAccessor(member, clazz.info.baseClasses)) {
              debugwarn("!!! is overridden val: "+member.fullLocationString)
            }
            else {
              // mixin field accessors
              val member1 = addMember(
                clazz,
                cloneBeforeErasure(mixinClass, clazz, member) //member.cloneSymbol(clazz)
                  setPos clazz.pos
                  resetFlag (DEFERRED | lateDEFERRED))
              // println("mixing in: "+ (member, member.info, member1.info))
              // atPhase(currentRun.erasurePhase){ 
              //   println("before erasure: "+ (member.info, member1.info))
              // }
              if (member.isLazy) {
                var init = implClass(mixinClass).info.decl(member.name)
                assert(init != NoSymbol, "Could not find initializer for " + member.name)
                initializer(member1) = init
              }
              if (!member.isSetter)
                member.tpe match {
                  case MethodType(Nil, ConstantType(_)) =>
                    // member is a constant; only getter is needed
                    ;
                  case MethodType(Nil, TypeRef(_, UnitClass, _)) =>
                    // member is a value of type unit. No field needed
                    ;
                  case _ => // otherwise mixin a field as well
                    // atPhase: the private field is moved to the implementation class by erasure,
                    // so it can no longer be found in the member's owner (the trait)
                    val accessed = atPhase(currentRun.picklerPhase)(member.accessed)
                    val sym = atPhase(currentRun.erasurePhase){ // #3857, need to retain info before erasure when cloning (since cloning only carries over the current entry in the type history)
                      clazz.newValue(member.pos, nme.getterToLocal(member.name)).setInfo(member.tpe.resultType) // so we have a type history entry before erasure
                    }
                    sym.updateInfo(member.tpe.resultType) // info at current phase
                    addMember(clazz,
                              sym
                                setFlag (LOCAL | PRIVATE | member.getFlag(MUTABLE | LAZY))
                                setFlag (if (!member.hasStableFlag) MUTABLE else 0)
                                setAnnotations accessed.annotations)
                }
            }
          } else if (member.isSuperAccessor) { // mixin super accessors
            val member1 = addMember(clazz, member.cloneSymbol(clazz)) setPos clazz.pos
            assert(member1.alias != NoSymbol, member1)
            val alias1 = rebindSuper(clazz, member.alias, mixinClass)
            member1.asInstanceOf[TermSymbol] setAlias alias1

          } else if (member.isMethod && member.isModule && member.hasNoFlags(LIFTED | BRIDGE)) {
            // mixin objects: todo what happens with abstract objects?
            addMember(clazz, member.cloneSymbol(clazz))
              .setPos(clazz.pos)
              .resetFlag(DEFERRED | lateDEFERRED)
          }
        }
      }

      for (mc <- clazz.mixinClasses)
        if (mc hasFlag lateINTERFACE) {
          // @SEAN: adding trait tracking so we don't have to recompile transitive closures
          unit.depends += mc
          addLateInterfaceMembers(mc)
          mixinTraitMembers(mc)
          mixinImplClassMembers(implClass(mc), mc)
        }
    }
  }

  /** The info transform for this phase does the following:
   *   - The parents of every class are mapped from implementation class to interface
   *   - Implementation classes become modules that inherit nothing
   *     and that define all.
   *
   *  @param sym ...
   *  @param tp  ...
   *  @return    ...
   */
  override def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) =>
      var parents1 = parents
      var decls1 = decls
      if (!clazz.isPackageClass) {
        atPhase(phase.next)(clazz.owner.info)
        if (clazz.isImplClass) {
          clazz setFlag lateMODULE
          var sourceModule = clazz.owner.info.decls.lookup(sym.name.toTermName)
          if (sourceModule != NoSymbol) {
            sourceModule setPos sym.pos
            sourceModule.flags = MODULE | FINAL
          } else {
            sourceModule = clazz.owner.newModule(
              sym.pos, sym.name.toTermName, sym.asInstanceOf[ClassSymbol])
            clazz.owner.info.decls enter sourceModule
          }
          sourceModule setInfo sym.tpe
          // Companion module isn't visible for anonymous class at this point anyway
          assert(clazz.sourceModule != NoSymbol || clazz.isAnonymousClass, 
            clazz + " has no sourceModule: sym = " + sym + " sym.tpe = " + sym.tpe)
          parents1 = List()
          decls1 = new Scope(decls.toList filter isImplementedStatically)
        } else if (!parents.isEmpty) {
          parents1 = parents.head :: (parents.tail map toInterface)
        }
      }
      //decls1 = atPhase(phase.next)(new Scope(decls1.toList))//debug
      if ((parents1 eq parents) && (decls1 eq decls)) tp
      else ClassInfoType(parents1, decls1, clazz)

    case MethodType(params, restp) =>
      toInterfaceMap(
        if (isImplementedStatically(sym)) {
          val ownerParam = sym.newSyntheticValueParam(toInterface(sym.owner.typeOfThis))
          MethodType(ownerParam :: params, restp)
        } else
          tp)

    case _ =>
      tp
  }

  import scala.collection._

  /** Return a map of single-use fields to the lazy value that uses them during initialization.
   *  Each field has to be private and defined in the enclosing class, and there must
   *  be exactly one lazy value using it.
   *
   *  Such fields will be nulled after the initializer has memoized the lazy value.
   */
  def singleUseFields(templ: Template): collection.Map[Symbol, List[Symbol]] = {
    val usedIn = new mutable.HashMap[Symbol, List[Symbol]] {
      override def default(key: Symbol) = Nil
    }

    object SingleUseTraverser extends Traverser {
      override def traverse(tree: Tree) {
        tree match {
          case Assign(lhs, rhs) => traverse(rhs) // assignments don't count
          case _ =>
            if (tree.hasSymbol && tree.symbol != NoSymbol) {
              val sym = tree.symbol
              if ((sym.hasAccessorFlag || (sym.isTerm && !sym.isMethod))
                  && sym.isPrivate
                  && !(currentOwner.isGetter && currentOwner.accessed == sym) // getter
                  && !definitions.isValueClass(sym.tpe.resultType.typeSymbol)
                  && sym.owner == templ.symbol.owner
                  && !sym.isLazy
                  && !tree.isDef) {
                log("added use in: " + currentOwner + " -- " + tree)
                usedIn(sym) ::= currentOwner

              }
            }
            super.traverse(tree)          
        }
      }
    }
    SingleUseTraverser(templ)
    log("usedIn: " + usedIn)
    usedIn filter {
      case (_, member :: Nil) => member.isValue && member.isLazy
      case _                  => false
    }
  }

// --------- term transformation -----------------------------------------------

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new MixinTransformer(unit)

  class MixinTransformer(unit : CompilationUnit) extends Transformer {

    /** Within a static implementation method: the parameter referring to the
     *  current object undefined everywhere else.
     */
    private var self: Symbol = _

    /** The rootContext used for typing */
    private val rootContext =
      erasure.NoContext.make(EmptyTree, RootClass, new Scope)

    /** The typer */
    private var localTyper: erasure.Typer = _
    private def typedPos(pos: Position)(tree: Tree) = localTyper typed { atPos(pos)(tree) }

    /** Map lazy values to the fields they should null after initialization. */
    private var lazyValNullables: mutable.MultiMap[Symbol, Symbol] = _

    import scala.collection._
    
    /** Map a field symbol to a unique integer denoting its position in the class layout.
     *  For each class, fields defined by the class come after inherited fields. Mixed-in
     *  fields count as fields defined by the class itself.
     */
    private val fieldOffset = perRunCaches.newMap[Symbol, Int]()

    /** The first transform; called in a pre-order traversal at phase mixin
     *  (that is, every node is processed before its children).
     *  What transform does:
     *   - For every non-trait class, add all mixed in members to the class info.
     *   - For every trait, add all late interface members to the class info
     *   - For every static implementation method:
     *       - remove override flag
     *       - create a new method definition that also has a `self` parameter
     *         (which comes first) Iuli: this position is assumed by tail call elimination 
     *         on a different receiver. Storing a new 'this' assumes it is located at
     *         index 0 in the local variable table. See 'STORE_THIS' and GenJVM/GenMSIL.
     *   - Map implementation class types in type-apply's to their interfaces
     *   - Remove all fields in implementation classes
     */
    private def preTransform(tree: Tree): Tree = {
      val sym = tree.symbol      
      tree match {
        case Template(parents, self, body) =>
          localTyper = erasure.newTyper(rootContext.make(tree, currentOwner))
          atPhase(phase.next)(currentOwner.owner.info)//todo: needed?
          
          if (!currentOwner.isTrait && !isValueClass(currentOwner))
            addMixedinMembers(currentOwner, unit)
          else if (currentOwner hasFlag lateINTERFACE)
            addLateInterfaceMembers(currentOwner)
            
          tree
        case DefDef(mods, name, tparams, List(vparams), tpt, rhs) =>
          if (currentOwner.isImplClass) {
            if (isImplementedStatically(sym)) {
              sym setFlag notOVERRIDE
              self = sym.newValue(sym.pos, nme.SELF) 
                .setFlag(PARAM)
                .setInfo(toInterface(currentOwner.typeOfThis));
              val selfdef = ValDef(self) setType NoType
              treeCopy.DefDef(tree, mods, name, tparams, List(selfdef :: vparams), tpt, rhs)
            } else {
              EmptyTree
            }
          } else {
            if (currentOwner.isTrait && sym.isSetter && !atPhase(currentRun.picklerPhase)(sym.isDeferred)) {
              sym.addAnnotation(AnnotationInfo(TraitSetterAnnotationClass.tpe, List(), List()))
            }
            tree
          }
        case Apply(tapp @ TypeApply(fn, List(arg)), List()) =>
          if (arg.tpe.typeSymbol.isImplClass) {
            val ifacetpe = toInterface(arg.tpe)
            arg.tpe = ifacetpe
            tapp.tpe = MethodType(List(), ifacetpe)
            tree.tpe = ifacetpe
          }
          tree
        case ValDef(_, _, _, _) if currentOwner.isImplClass =>
          EmptyTree
        case _ =>
          tree
      }
    }
         
    /** Create an identifier which references self parameter.
     *
     *  @param pos ...
     */
    private def selfRef(pos: Position) =
      gen.mkAttributedIdent(self) setPos pos

    /** Replace a super reference by this or the self parameter, depending
     *  on whether we are in an implementation class or not.
     *  Leave all other trees unchanged. 
     */
    private def transformSuper(tree: Tree) = tree match {
      case Super(qual, _) => 
        transformThis(qual)
      case _ => 
        tree
    }
    
    /** Replace a this reference to the current implementation class by the self 
     *  parameter. Leave all other trees unchanged.
     */
    private def transformThis(tree: Tree) = tree match {
      case This(_) if tree.symbol.isImplClass =>
        assert(tree.symbol == currentOwner.enclClass)
        selfRef(tree.pos)
      case _ =>
        tree
    }

    /** Create a static reference to given symbol <code>sym</code> of the
     *  form <code>M.sym</code> where M is the symbol's implementation module.
     */
    private def staticRef(sym: Symbol): Tree = {
      sym.owner.info  //todo: needed?
      sym.owner.owner.info //todo: needed?
      if (sym.owner.sourceModule == NoSymbol)
        assert(false, "" + sym + " in " + sym.owner + " in " + sym.owner.owner +
                      " " + sym.owner.owner.info.decls)//debug
      REF(sym.owner.sourceModule) DOT sym
    }
      
    @inline private def bitmapOperation[T](field: Symbol, transientCase: => T, privateCase: => T, rest: => T): T =
      if (field.accessed.hasAnnotation(TransientAttr))
        transientCase
      else if (field.hasFlag(PRIVATE | notPRIVATE))
        privateCase
      else
        rest

    /** Add all new definitions to a non-trait class 
     *  These fall into the following categories:
     *    - for a trait interface: 
     *       - abstract accessors for all fields in the implementation class
     *    - for a non-trait class:
     *       - A field for every in a mixin class 
     *       - Setters and getters for such fields
     *           - getters for mixed in lazy fields are completed 
     *       - module variables and module creators for every module in a mixin class
     *         (except if module is lifted -- in this case the module variable
     *          is local to some function, and the creator method is static.)
     *       - A super accessor for every super accessor in a mixin class
     *       - Forwarders for all methods that are implemented statically
     *  All superaccessors are completed with right-hand sides (@see completeSuperAccessor)
     *  @param clazz  The class to which definitions are added
     */
    private def addNewDefs(clazz: Symbol, stats: List[Tree]): List[Tree] = {
      val newDefs = new ListBuffer[Tree]

      /** Attribute given tree and anchor at given position */
      def attributedDef(pos: Position, tree: Tree): Tree = {
        debuglog("add new def to " + clazz + ": " + tree)
        typedPos(pos)(tree)
      }

      /** The position of given symbol, or, if this is undefined, 
       *  the position of the current class. */
      def position(sym: Symbol) =
        if (sym.pos == NoPosition) clazz.pos else sym.pos

      /** Add tree at given position as new definition */
      def addDef(pos: Position, tree: Tree) {
        newDefs += attributedDef(pos, tree)
      }

      /** Add new method definition.
       *
       *  @param sym   The method
       *  @param rhs   A function that maps formal parameters to the method's
       *               right-hand side
       */
      def addDefDef(sym: Symbol, rhs: List[Symbol] => Tree) {
        addDef(position(sym), DefDef(sym, rhs(sym.paramss.head)))
      }

      /** Add `newdefs` to `stats`, removing any abstract method definitions
       *  in <code>stats</code> that are matched by some symbol defined in
       *  <code>newDefs</code>.
       */
      def add(stats: List[Tree], newDefs: List[Tree]) = {
        val newSyms = newDefs map (_.symbol)
        def isNotDuplicate(tree: Tree) = tree match {
          case DefDef(_, _, _, _, _, _) =>
            val sym = tree.symbol
            !(sym.isDeferred &&
              (newSyms exists (nsym => nsym.name == sym.name && (nsym.tpe matches sym.tpe))))
          case _ =>
            true
        }
        if (newDefs.isEmpty) stats
        else newDefs ::: stats.filter(isNotDuplicate)
      }
      
      def addDeferredBitmap(clazz: Symbol, tree: Tree) {
          // Append the set of deffered defs
          deferredBitmaps(clazz) = typedPos(clazz.pos)(tree)::deferredBitmaps.getOrElse(clazz, List())
      }

      /** If `stat` is a superaccessor, complete it by adding a right-hand side.
       *  Note: superaccessors are always abstract until this point. 
       *   The method to call in a superaccessor is stored in the accessor symbol's alias field.
       *  The rhs is: 
       *    super.A(xs)  where A is the super accessor's alias and xs are its formal parameters.
       *  This rhs is typed and then mixin transformed.
       */
      def completeSuperAccessor(stat: Tree) = stat match {
        case DefDef(mods, name, tparams, List(vparams), tpt, EmptyTree)
        if (stat.symbol.isSuperAccessor) =>
          val rhs0 = (Super(clazz, tpnme.EMPTY) DOT stat.symbol.alias)(vparams map (v => Ident(v.symbol)): _*)
          val rhs1 = localTyper.typed(atPos(stat.pos)(rhs0), stat.symbol.tpe.resultType)
          val rhs2 = atPhase(currentRun.mixinPhase)(transform(rhs1))
          debuglog("complete super acc " + stat.symbol + stat.symbol.locationString +
                " " + rhs1 + " " + stat.symbol.alias + stat.symbol.alias.locationString +
                "/" + stat.symbol.alias.owner.hasFlag(lateINTERFACE))//debug
          treeCopy.DefDef(stat, mods, name, tparams, List(vparams), tpt, rhs2)
        case _ =>
          stat
      }

      import lazyVals._
      
      /**
       *  Private or transient lazy vals use bitmaps that are private for the class context,
       *  unlike public or protected vals, which can use inherited bitmaps.
       *  Similarly fields in the checkinit mode use private bitmaps.
       */
      def localBitmapField(field: Symbol) =
        field.accessed.hasAnnotation(TransientAttr) || field.hasFlag(PRIVATE | notPRIVATE) || checkinitField(field)

      /**
       *  Return the bitmap field for 'offset'. Depending on the hierarchy it is possible to reuse
       *  the bitmap of its parents. If that does not exist yet we create one.
       */ 
      def bitmapFor(clazz0: Symbol, offset: Int, field: Symbol, searchParents:Boolean = true): Symbol = {
        def bitmapLazyName: Name =
          bitmapOperation(field, nme.bitmapNameForTransient(offset / FLAGS_PER_WORD),
                          nme.bitmapNameForPrivate(offset / FLAGS_PER_WORD),
                          nme.bitmapName(offset / FLAGS_PER_WORD))
        def bitmapCheckinitName: Name =
          bitmapOperation(field, nme.bitmapNameForCheckinitTransient(offset / FLAGS_PER_WORD),
                          nme.bitmapNameForCheckinit(offset / FLAGS_PER_WORD),
                          nme.bitmapNameForCheckinit(offset / FLAGS_PER_WORD))
        val checkinitField = !field.isLazy
        val bitmapName = if (checkinitField) bitmapCheckinitName else bitmapLazyName
 
        def createBitmap: Symbol = {
          val sym = clazz0.newVariable(clazz0.pos, bitmapName).setInfo(IntClass.tpe)
          atPhase(currentRun.typerPhase) {
            sym addAnnotation AnnotationInfo(VolatileAttr.tpe, Nil, Nil)
          }
          
          bitmapOperation(field,
            {sym.addAnnotation(AnnotationInfo(TransientAttr.tpe, Nil, Nil)); sym.setFlag(PRIVATE | LOCAL)},
            sym.setFlag(PRIVATE | LOCAL),
            sym.setFlag(if (checkinitField) (PRIVATE | LOCAL) else PROTECTED))

          clazz0.info.decls.enter(sym)
          if (clazz0 == clazz)
            addDef(clazz.pos, VAL(sym) === ZERO)
          else {
            //FIXME: the assertion below will not work because of the way bitmaps are added.
            // They should be added during infoTransform, so that in separate compilation, bitmap
            // is a member of clazz and doesn't fail the condition couple lines below.
            // This works, as long as we assume that the previous classes were compiled correctly.
            //assert(clazz0.sourceFile != null)
            addDeferredBitmap(clazz0, VAL(sym) === ZERO)
          }
          sym
        }
        var sym = clazz0.info.member(bitmapName)
        assert(!sym.hasFlag(OVERLOADED))
        if (sym == NoSymbol) {
          if (searchParents && !localBitmapField(field))
            bitmapForParents(clazz0, offset, field) match {
              case Some(bitmap) =>
                sym = bitmap
              case None =>
                sym = createBitmap
            }
          else
            sym = createBitmap
        }
        sym
      }
      
      def bitmapForParents(clazz0: Symbol, offset: Int, valSym: Symbol): Option[Symbol] = {
        def requiredBitmaps(fs: Int): Int = if (fs == 0) -1 else (fs - 1) / FLAGS_PER_WORD
        
        var res:Option[Symbol] = None
        val bitmapNum = offset / FLAGS_PER_WORD
        
        
        // filter private and transient
        // since we do not inherit normal values (in checkinit mode) also filter them out
        for (cl <- clazz0.info.baseClasses.tail.filter(c => !c.isTrait && !c.hasFlag(JAVA))
             if res == None) {
          val fields0 = usedBits(cl)
         
          if (requiredBitmaps(fields0) < bitmapNum) { 
            val fields1 = cl.info.decls.filter(decl => fieldWithBitmap(decl) && !localBitmapField(decl)).size
            if (requiredBitmaps(fields0 + fields1) >= bitmapNum)
              res = Some(bitmapFor(cl, offset, valSym, false))
            else return None // Don't waste time, since we won't find bitmap anyway
          }
        }
        res
      }

      /** Return an (untyped) tree of the form 'Clazz.this.bmp = Clazz.this.bmp | mask'. */
      def mkSetFlag(clazz: Symbol, offset: Int, valSym: Symbol): Tree = {
        val bmp   = bitmapFor(clazz, offset, valSym)
        val mask  = LIT(1 << (offset % FLAGS_PER_WORD))
        def x     = This(clazz) DOT bmp
        
        x === (x INT_| mask)
      }
      
      /** Return an (untyped) tree of the form 'clazz.this.bitmapSym & mask (==|!=) 0', the
       *  precise comparison operator depending on the value of 'equalToZero'.
       */
      def mkTest(clazz: Symbol, mask: Tree, bitmapSym: Symbol, equalToZero: Boolean): Tree = {
        def lhs = (This(clazz) DOT bitmapSym) INT_& mask
        if (equalToZero)  lhs INT_== ZERO
        else              lhs INT_!= ZERO
      }

      /** return a 'lazified' version of rhs. It uses double-checked locking to ensure
       *  initialization is performed at most once. Private fields used only in this
       *  initializer are subsequently set to null.
       * 
       *  @param clazz The class symbol
       *  @param init The tree which initializes the field ( f = <rhs> )
       *  @param fieldSym The symbol of this lazy field
       *  @param offset The offset of this field in the flags bitmap
       *
       *  The result will be a tree of the form
       *  {
       *    if ((bitmap$n & MASK) == 0) {
       *       synchronized(this) {
       *         if ((bitmap$n & MASK) == 0) {
       *           init // l$ = <rhs>
       *           bitmap$n = bimap$n | MASK
       *         }
       *       }
       *       this.f1 = null
       *       ... this.fn = null 
       *    }
       *    l$
       *  }
       *  where bitmap$n is an int value acting as a bitmap of initialized values. It is
       *  the 'n' is (offset / 32), the MASK is (1 << (offset % 32)).
       */
      def mkLazyDef(clazz: Symbol, lzyVal: Symbol, init: List[Tree], retVal: Tree, offset: Int): Tree = {
        def nullify(sym: Symbol): Tree = {
          val sym1 = if (sym.hasAccessorFlag) sym.accessed else sym
          Select(This(clazz), sym1) === LIT(null)
        }
        
        val bitmapSym = bitmapFor(clazz, offset, lzyVal)
        val mask      = LIT(1 << (offset % FLAGS_PER_WORD))
        def cond      = mkTest(clazz, mask, bitmapSym, true)
        val nulls     = (lazyValNullables(lzyVal).toList sortBy (_.id) map nullify)
        def syncBody  = init ::: List(mkSetFlag(clazz, offset, lzyVal), UNIT)

        log("nulling fields inside " + lzyVal + ": " + nulls)
        val result    = gen.mkDoubleCheckedLocking(clazz, cond, syncBody, nulls)
        typedPos(init.head.pos)(BLOCK(result, retVal))
      }
      
      def mkInnerClassAccessorDoubleChecked(attrThis: Tree, rhs: Tree): Tree =
        rhs match {
          case Block(List(assign), returnTree) =>
            val Assign(moduleVarRef, _) = assign
            val cond = Apply(Select(moduleVarRef, nme.eq),List(Literal(Constant(null))))
            val doubleSynchrTree = gen.mkDoubleCheckedLocking(attrThis, cond, List(assign), Nil)
            Block(List(doubleSynchrTree), returnTree)
          case _ =>
            assert(false, "Invalid getter " + rhs + " for module in class " + clazz)
            EmptyTree
        }

      def mkCheckedAccessor(clazz: Symbol, retVal: Tree, offset: Int, pos: Position, fieldSym: Symbol): Tree = {
        val bitmapSym = bitmapFor(clazz, offset, fieldSym.getter(fieldSym.owner))
        val mask      = LIT(1 << (offset % FLAGS_PER_WORD))
        val msg       = "Uninitialized field: " + unit.source + ": " + pos.line
        val result    = 
          IF (mkTest(clazz, mask, bitmapSym, false)) .
            THEN (retVal) .
            ELSE (THROW(UninitializedErrorClass, LIT(msg)))
        
        typedPos(pos)(BLOCK(result, retVal))
      }
       
      /** Complete lazy field accessors. Applies only to classes, for it's own (non inherited) lazy fields.
       *  If 'checkinit' is enabled, getters that check for the initialized bit are generated, and
       *  the class constructor is changed to set the initialized bits.
       */
      def addCheckedGetters(clazz: Symbol, stats: List[Tree]): List[Tree] = {

        val stats1 = for (stat <- stats; sym = stat.symbol) yield stat match {
          case DefDef(mods, name, tp, vp, tpt, rhs) 
            if sym.isLazy && rhs != EmptyTree && !clazz.isImplClass =>
              assert(fieldOffset.isDefinedAt(sym))
              val rhs1 = if (sym.tpe.resultType.typeSymbol == UnitClass) 
                mkLazyDef(clazz, sym, List(rhs), UNIT, fieldOffset(sym))
              else {
                val Block(stats, res) = rhs
                mkLazyDef(clazz, sym, stats, Select(This(clazz), res.symbol), fieldOffset(sym))
              }
              treeCopy.DefDef(stat, mods, name, tp, vp, tpt, rhs1)

          case DefDef(mods, name, tp, vp, tpt, rhs) 
            if needsInitFlag(sym) && rhs != EmptyTree && !clazz.isImplClass && !clazz.isTrait =>
              assert(fieldOffset.isDefinedAt(sym))
              val rhs1 = (mkCheckedAccessor(clazz, _: Tree, fieldOffset(sym), stat.pos, sym))(
                if (sym.tpe.resultType.typeSymbol == UnitClass) UNIT else rhs
              )
              treeCopy.DefDef(stat, mods, name, tp, vp, tpt, rhs1)

          case DefDef(mods, name, tp, vp, tpt, rhs) if sym.isConstructor =>
            treeCopy.DefDef(stat, mods, name, tp, vp, tpt, addInitBits(clazz, rhs))

          case DefDef(mods, name, tp, vp, tpt, rhs)
            if settings.checkInit.value && !clazz.isTrait && sym.isSetter =>
              val getter = sym.getter(clazz)
              if (needsInitFlag(getter) && fieldOffset.isDefinedAt(getter))
                treeCopy.DefDef(stat, mods, name, tp, vp, tpt,
                        Block(List(rhs, localTyper.typed(mkSetFlag(clazz, fieldOffset(getter), getter))), UNIT))
              else 
                stat
          case DefDef(mods, name, tp, vp, tpt, rhs)
            if sym.isModule && (!clazz.isTrait || clazz.isImplClass) && !sym.hasFlag(BRIDGE) =>
              val attrThis =
                if (clazz.isImplClass) {
                  gen.mkAttributedIdent(vp.head.head.symbol)
                  // Martin to Hubert I think this can be replaced by selfRef(tree.pos)
                } else 
                  gen.mkAttributedThis(clazz)
              val rhs1 = mkInnerClassAccessorDoubleChecked(attrThis, rhs)
              treeCopy.DefDef(stat, mods, name, tp, vp, tpt, typedPos(stat.pos)(rhs1))
          case _ => stat
        }
        stats1
      }

      /** Does this field require an initialized bit?
       *  Note: fields of classes inheriting DelayedInit are not checked.
       *        This is because the they are neither initialized in the constructor
       *        nor do they have a setter (not if they are vals anyway). The usual
       *        logic for setting bitmaps does therefor not work for such fields.
       *        That's why they are excluded.
       */
      def needsInitFlag(sym: Symbol) = {
        val res = (settings.checkInit.value 
           && sym.isGetter 
           && !sym.isInitializedToDefault
           && !sym.hasFlag(PARAMACCESSOR | SPECIALIZED | LAZY)
           && !sym.accessed.hasFlag(PRESUPER)
           && !sym.isOuterAccessor
           && !(sym.owner isSubClass DelayedInitClass))
        
//        if (settings.debug.value) {
//          log("needsInitFlag(" + sym.fullName + "): " + res)
//          log("\tsym.isGetter: " + sym.isGetter)
//          log("\t!isInitializedToDefault: " + !sym.isInitializedToDefault + sym.hasFlag(DEFAULTINIT) + sym.hasAccessorFlag + sym.isTerm)
//          log("\t!sym.isParamAccessor: " + !sym.isParamAccessor)
//          //println("\t!sym.accessed.hasFlag(PRESUPER): " + !sym.accessed.hasFlag(PRESUPER))
//          log("\t!sym.isOuterAccessor: " + !sym.isOuterAccessor)
//        }
        
        res
      }
      
      /** Adds statements to set the 'init' bit for each field initialized 
       * in the body of a constructor. 
       */
      def addInitBits(clazz: Symbol, rhs: Tree): Tree = {
        new Transformer {
          override def transformStats(stats: List[Tree], exprOwner: Symbol) = {
            val stats1 = stats flatMap { stat => stat match {
              case Assign(lhs @ Select(This(_), _), rhs) =>
                val sym = clazz.info.decl(nme.getterName(lhs.symbol.name))
                  .suchThat(_.isGetter)
                if (rhs == EmptyTree)
                  List()
                else if (sym != NoSymbol && needsInitFlag(sym) && fieldOffset.isDefinedAt(sym)) {
                  log("adding checked getter for: " + sym + " " + Flags.flagsToString(lhs.symbol.flags))
                  List(stat, localTyper.typed(mkSetFlag(clazz, fieldOffset(sym), sym)))
                } else {
                  List(stat)
                }
              case Apply(setter @ Select(Ident(self), _), List(EmptyTree)) if setter.symbol.isSetter =>
                // remove initialization for default values
                List()
              case _ => List(stat)
            }
            }
            super.transformStats(stats1, exprOwner)
          }
        }.transform(rhs)
      }
      
      def fieldWithBitmap(field: Symbol) = {
        field.info // ensure that nested objects are transformed
        // For checkinit consider normal value getters
        // but for lazy values only take into account lazy getters
        field.isLazy && field.isMethod && !field.isDeferred
      }

      def checkinitField(field: Symbol) = 
        needsInitFlag(field) && !field.isDeferred
      
      /**
       * Return the number of bits used by superclass fields.
       */
      def usedBits(clazz0: Symbol): Int = {
        def needsBitmap(field: Symbol) = field.owner != clazz0 && fieldWithBitmap(field)
        var bits = 0
        for {
          cl <- clazz0.info.baseClasses.tail
          if !cl.isTrait && !cl.hasFlag(JAVA)
          field <- cl.info.decls.iterator
          if needsBitmap(field) && !localBitmapField(field)
        } bits += 1

        bits
      }
      
      /** Fill the map from fields to offset numbers.
       *  Instead of field symbols, the map keeps their getter symbols. This makes
       *  code generation easier later.
       */
      def buildFieldPositions(clazz0: Symbol) {
        var fields = usedBits(clazz0)
        var fieldsPrivate = 0
        var fieldsTransient = 0
        var fieldsCheckinit = 0
        var fieldsCheckinitTransient = 0

        for (f <- clazz0.info.decls.iterator) {
          debuglog(f.fullName + " -> " + fields)

          if (fieldWithBitmap(f)) {
            val (idx, _) =
              bitmapOperation(f, (fieldsTransient, fieldsTransient += 1),
                                 (fieldsPrivate, fieldsPrivate += 1),
                                 (fields, fields += 1))
            fieldOffset(f) = idx
          } else if (checkinitField(f)) {
            // bitmaps for checkinit fields are not inherited
            val (idx, _) = 
              bitmapOperation(f, (fieldsCheckinitTransient, fieldsCheckinitTransient += 1),
                                 (fieldsCheckinit, fieldsCheckinit += 1),
                                 (fieldsCheckinit, fieldsCheckinit += 1))
            fieldOffset(f) = idx
          }
        }        
      }
      
      // begin addNewDefs      
      buildFieldPositions(clazz)
      var stats1 = addCheckedGetters(clazz, stats)
      
      // add deffered bitmaps
      deferredBitmaps.remove(clazz) match {
          case Some(deferred) =>
            stats1 = add(stats1, deferred)
          case None =>
      }

      // for all symbols `sym` in the class definition, which are mixed in:
      for (sym <- clazz.info.decls) {
        if (sym hasFlag MIXEDIN) {
          if (clazz hasFlag lateINTERFACE) {
            // if current class is a trait interface, add an abstract method for accessor `sym`
            addDefDef(sym, vparamss => EmptyTree)
          } else if (!clazz.isTrait) { 
            // if class is not a trait add accessor definitions
            if ((sym hasFlag ACCESSOR) && 
                (!(sym hasFlag DEFERRED) || (sym hasFlag lateDEFERRED))) { 
              // add accessor definitions
              addDefDef(sym, vparams => {
                val accessedRef = sym.tpe match {
                  case MethodType(List(), ConstantType(c)) => Literal(c)
                  case _ => 
                    // if it is a mixed-in lazy value, complete the accessor
                    if (sym.isLazy && sym.isGetter) {
                      val rhs1 = 
                        if (sym.tpe.resultType.typeSymbol == UnitClass)
                          mkLazyDef(clazz, sym, List(Apply(staticRef(initializer(sym)), List(gen.mkAttributedThis(clazz)))), UNIT, fieldOffset(sym))
                        else {
                          val assign = atPos(sym.pos) {
                            Assign(Select(This(sym.accessed.owner), sym.accessed) /*gen.mkAttributedRef(sym.accessed)*/ ,
                                Apply(staticRef(initializer(sym)), gen.mkAttributedThis(clazz) :: Nil))
                          }
                          mkLazyDef(clazz, sym, List(assign), Select(This(clazz), sym.accessed), fieldOffset(sym))
                        }
                      rhs1
                    } else if (sym.getter(sym.owner).tpe.resultType.typeSymbol == UnitClass) {
                      UNIT
                    } else {
                      Select(This(clazz), sym.accessed)
                    }
                }
                if (sym.isSetter) {
                  val isOverriddenSetter = 
                    nme.isTraitSetterName(sym.name) && {
                      val other = sym.nextOverriddenSymbol
                      (other != NoSymbol) && isOverriddenAccessor(other.getter(other.owner), clazz.info.baseClasses)
                    }
                  if (isOverriddenSetter) UNIT
                  else accessedRef match {
                    case Literal(_) => accessedRef
                    case _ =>
                      val init = Assign(accessedRef, Ident(vparams.head)) 
                      val getter = sym.getter(clazz)                      
                      if (needsInitFlag(getter))
                        Block(List(init, mkSetFlag(clazz, fieldOffset(getter), getter)), UNIT)
                      else
                        init
                  }
                } else if (needsInitFlag(sym)) {
                  mkCheckedAccessor(clazz,  accessedRef, fieldOffset(sym), sym.pos, sym)
                } else
                  gen.mkCheckInit(accessedRef)
              })
            } else if (sym.isModule && !(sym hasFlag LIFTED | BRIDGE)) { 
              // add modules  
              val vdef = gen.mkModuleVarDef(sym) 
              addDef(position(sym), vdef)
              
              val rhs  = gen.newModule(sym, vdef.symbol.tpe)
              val assignAndRet = gen.mkAssignAndReturn(vdef.symbol, rhs)
              val attrThis = gen.mkAttributedThis(clazz)
              val rhs1 = mkInnerClassAccessorDoubleChecked(attrThis, assignAndRet)
              addDef(position(sym), DefDef(sym, rhs1))
            } else if (!sym.isMethod) {
              // add fields
              addDef(position(sym), ValDef(sym))
            } else if (sym.isSuperAccessor) {
              // add superaccessors
              addDefDef(sym, vparams => EmptyTree)
            } else {
              // add forwarders
              assert(sym.alias != NoSymbol, sym)
              addDefDef(sym, vparams =>
                Apply(staticRef(sym.alias), gen.mkAttributedThis(clazz) :: (vparams map Ident)))
            }
          }
        }
      }
      stats1 = add(stats1, newDefs.toList)
      if (!clazz.isTrait) stats1 = stats1 map completeSuperAccessor
      stats1
    }

    private def nullableFields(templ: Template) = {
      val nullables = new mutable.HashMap[Symbol, mutable.Set[Symbol]] with mutable.MultiMap[Symbol, Symbol] {
        override def default(key: Symbol) = mutable.Set.empty
      }

      // if there are no lazy fields, take the fast path and save a traversal of the whole AST
      if (templ.symbol.owner.info.decls.exists(_.isLazy)) {
        // check what fields can be nulled for
        val uses = singleUseFields(templ)
        for ((field, users) <- uses; lazyFld <- users) {
          nullables.addBinding(lazyFld, field)
        }
      }
      nullables
    }

    /** The transform that gets applied to a tree after it has been completely
     *  traversed and possible modified by a preTransform.
     *  This step will
     *    - change every node type that refers to an implementation class to its 
     *      corresponding interface, unless the node's symbol is an implementation class.
     *    - change parents of templates to conform to parents in the symbol info
     *    - add all new definitions to a class or interface
     *    - remove widening casts
     *    - change calls to methods which are defined only in implementation classes
     *      to static calls of methods in implementation modules (@see staticCall)
     *    - change super calls to methods in implementation classes to static calls
     *      (@see staticCall)
     *    - change `this` in implementation modules to references to the self parameter
     *    - refer to fields in some implementation class vie an abstract method in the interface.
     */
    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol

      // change every node type that refers to an implementation class to its
      // corresponding interface, unless the node's symbol is an implementation class.
      if (tree.tpe.typeSymbol.isImplClass &&
          ((tree.symbol eq null) || !tree.symbol.isImplClass))
        tree.tpe = toInterface(tree.tpe);

      tree match {
        case Template(parents, self, body) =>
          // change parents of templates to conform to parents in the symbol info
          val parents1 = currentOwner.info.parents map (t => TypeTree(t) setPos tree.pos)

          lazyValNullables = nullableFields(tree.asInstanceOf[Template])
          // add all new definitions to current class or interface
          val body1 = addNewDefs(currentOwner, body)

          treeCopy.Template(tree, parents1, self, body1)

        case Apply(TypeApply(sel @ Select(qual, name), List(targ)), List())
        if (tree.symbol == Object_asInstanceOf && (qual.tpe <:< targ.tpe)) =>
          // remove widening casts
          qual

        case Apply(Select(qual, _), args) =>
          /** Changes <code>qual.m(args)</code> where m refers to an implementation
           *  class method to Q.m(S, args) where Q is the implementation module of
           *  <code>m</code> and S is the self parameter for the call, which
           *  is determined as follows:
           *     - if qual != super, qual itself
           *     - if qual == super, and we are in an implementation class,
           *       the current self parameter.
           *     - if qual == super, and we are not in an implementation class, `this`
           */
          def staticCall(target: Symbol) = {
            if (target == NoSymbol)
              assert(false, "" + sym + ":" + sym.tpe + " " + sym.owner + " " + implClass(sym.owner) + " " + implClass(sym.owner).info.member(sym.name) + " " + atPhase(phase.prev)(implClass(sym.owner).info.member(sym.name).tpe) + " " + phase);//debug
            
            typedPos(tree.pos)(Apply(staticRef(target), transformSuper(qual) :: args))
          }
          if (isStaticOnly(sym)) {
            // change calls to methods which are defined only in implementation
            // classes to static calls of methods in implementation modules
            staticCall(sym)
          } else qual match {
            case Super(_, mix) =>
              // change super calls to methods in implementation classes to static calls.
              // Transform references super.m(args) as follows:
              //  - if `m` refers to a trait, insert a static call to the corresponding static
              //    implementation
              //  - otherwise return tree unchanged
              if (mix == tpnme.EMPTY && currentOwner.enclClass.isImplClass)
                assert(false, "illegal super in trait: " + currentOwner.enclClass + " " + tree);
              if (sym.owner hasFlag lateINTERFACE) {
                if (sym.hasAccessorFlag) {
                  assert(args.isEmpty)
                  val sym1 = sym.overridingSymbol(currentOwner.enclClass)
                  typedPos(tree.pos)((transformSuper(qual) DOT sym1)())
                } else {
                  staticCall(atPhase(phase.prev)(sym.overridingSymbol(implClass(sym.owner))))
                }
              } else {
                assert(!currentOwner.enclClass.isImplClass)
                tree
              }
            case _ =>
              tree
          }

        case This(_) =>
          transformThis(tree)

        case Select(Super(_, _), name) =>
          tree

        case Select(qual, name) if sym.owner.isImplClass && !isStaticOnly(sym) =>
          assert(!sym.isMethod, "no method allowed here: %s%s %s".format(sym, sym.isImplOnly, flagsToString(sym.flags)))

          // refer to fields in some implementation class via an abstract
          // getter in the interface.
          val iface = toInterface(sym.owner.tpe).typeSymbol
          val getter = sym.getter(iface)
          assert(getter != NoSymbol)
          typedPos(tree.pos)((qual DOT getter)())

        case Assign(Apply(lhs @ Select(qual, _), List()), rhs) =>
          // assign to fields in some implementation class via an abstract
          // setter in the interface.
          def setter = lhs.symbol.setter(
            toInterface(lhs.symbol.owner.tpe).typeSymbol, 
            needsExpandedSetterName(lhs.symbol)
          ) setPos lhs.pos
          
          typedPos(tree.pos) { (qual DOT setter)(rhs) }

        case _ =>
          tree
      }
    }

    /** The main transform method.
     *  This performs pre-order traversal preTransform at mixin phase;
     *  when coming back, it performs a postTransform at phase after.
     */
    override def transform(tree: Tree): Tree = {
      try { //debug
        val outerTyper = localTyper
        val tree1 = super.transform(preTransform(tree))
        val res = atPhase(phase.next)(postTransform(tree1))
        // needed when not flattening inner classes. parts after an
        // inner class will otherwise be typechecked with a wrong scope
        localTyper = outerTyper
        res
      } catch {
        case ex: Throwable =>
          if (settings.debug.value) Console.println("exception when traversing " + tree)
          throw ex
      }
    }
  }
}
