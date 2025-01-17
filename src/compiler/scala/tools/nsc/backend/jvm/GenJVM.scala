/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Iulian Dragos
 */


package scala.tools.nsc
package backend.jvm

import java.io.{ DataOutputStream, OutputStream }
import java.nio.ByteBuffer
import scala.collection.{ mutable, immutable }
import scala.reflect.internal.pickling.{ PickleFormat, PickleBuffer }
import scala.tools.nsc.util.ScalaClassLoader
import scala.tools.nsc.symtab._
import scala.reflect.internal.ClassfileConstants._
import ch.epfl.lamp.fjbg._
import JAccessFlags._
import JObjectType.{ JAVA_LANG_STRING, JAVA_LANG_OBJECT }
import java.util.jar.{ JarEntry, JarOutputStream }
import scala.tools.nsc.io.AbstractFile

/** This class ...
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 * 
 */
abstract class GenJVM extends SubComponent with GenJVMUtil with GenAndroid with BytecodeWriters {
  import global._
  import icodes._
  import icodes.opcodes._
  import definitions.{
    NullClass, RuntimeNullClass, NothingClass, RuntimeNothingClass,
    AnyClass, ObjectClass, ThrowsClass, ThrowableClass, ClassfileAnnotationClass,
    SerializableClass, StringClass, ClassClass, FunctionClass,
    DeprecatedAttr, SerializableAttr, SerialVersionUIDAttr, VolatileAttr,
    TransientAttr, CloneableAttr, RemoteAttr,
    hasJavaMainMethod
  }

  val phaseName = "jvm"

  /** Create a new phase */
  override def newPhase(p: Phase): Phase = new JvmPhase(p)

  private def outputDirectory(sym: Symbol): AbstractFile = (
    settings.outputDirs.outputDirFor {
      atPhase(currentRun.flattenPhase.prev)(sym.sourceFile)
    }
  )
  private def getFile(base: AbstractFile, cls: JClass, suffix: String): AbstractFile = {
    var dir = base
    val pathParts = cls.getName().split("[./]").toList
    for (part <- pathParts.init) {
      dir = dir.subdirectoryNamed(part)
    }
    dir.fileNamed(pathParts.last + suffix)
  }
  private def getFile(sym: Symbol, cls: JClass, suffix: String): AbstractFile =
    getFile(outputDirectory(sym), cls, suffix)

  /** JVM code generation phase
   */
  class JvmPhase(prev: Phase) extends ICodePhase(prev) {
    def name = phaseName
    override def erasedTypes = true
    def apply(cls: IClass) = sys.error("no implementation")

    def isJavaEntryPoint(clasz: IClass) = {
      val sym = clasz.symbol
      def fail(msg: String) = {
        clasz.cunit.warning(sym.pos, 
          sym.name + " has a main method, but " + sym.fullName('.') + " will not be a runnable program.\n" +
          "  " + msg + ", which means no static forwarder can be generated.\n"
          // TODO: make this next claim true, if possible 
          //   by generating valid main methods as static in module classes
          //   not sure what the jvm allows here
          // + "  You can still run the program by calling it as " + sym.javaSimpleName + " instead."
        )
        false
      }
      sym.hasModuleFlag && hasJavaMainMethod(sym) && {
        // At this point we've seen a module with a main method, so if this
        // doesn't turn out to be a valid entry point, issue a warning.
        val companion = sym.linkedClassOfClass
        if (companion.isTrait)
          fail("Its companion is a trait")
        else if (hasJavaMainMethod(companion) && !(sym isSubClass companion))
          fail("Its companion contains its own main method")
        // this is only because forwarders aren't smart enough yet
        else if (companion.tpe.member(nme.main) != NoSymbol)
          fail("Its companion contains its own main method (implementation restriction: no main is allowed, regardless of signature)")
        else
          true
      }
    }

    override def run() {
      // we reinstantiate the bytecode generator at each run, to allow the GC
      // to collect everything
      if (settings.debug.value)
        inform("[running phase " + name + " on icode]")

      if (settings.Xdce.value)
        for ((sym, cls) <- icodes.classes if inliner.isClosureClass(sym) && !deadCode.liveClosures(sym))
          icodes.classes -= sym

      // For predictably ordered error messages.
      val sortedClasses = classes.values.toList sortBy ("" + _.symbol.fullName)
      val entryPoints   = sortedClasses filter isJavaEntryPoint
      
      val bytecodeWriter = settings.outputDirs.getSingleOutput match {
        case Some(f) if f hasExtension "jar" =>
          // If no main class was specified, see if there's only one
          // entry point among the classes going into the jar.
          if (settings.mainClass.isDefault) {
            entryPoints map (_.symbol fullName '.') match {
              case Nil      =>
                log("No Main-Class designated or discovered.")
              case name :: Nil =>
                log("Unique entry point: setting Main-Class to " + name)
                settings.mainClass.value = name
              case names =>
                log("No Main-Class due to multiple entry points:\n  " + names.mkString("\n  "))
            }
          }
          else log("Main-Class was specified: " + settings.mainClass.value)

          new DirectToJarfileWriter(f.file)
          
        case _                               =>
          if (settings.Ygenjavap.isDefault) new ClassBytecodeWriter { }
          else new ClassBytecodeWriter with JavapBytecodeWriter { }
      }

      val codeGenerator = new BytecodeGenerator(bytecodeWriter)
      log("Created new bytecode generator for " + classes.size + " classes.")

      sortedClasses foreach { c =>
        try codeGenerator.genClass(c)
        catch {
          case e: JCode.CodeSizeTooBigException =>
            log("Skipped class %s because it has methods that are too long.".format(c))
        }
      }

      bytecodeWriter.close()
      classes.clear()
    }
  }

  var pickledBytes = 0 // statistics

  /**
   * Java bytecode generator.
   *
   */
  class BytecodeGenerator(bytecodeWriter: BytecodeWriter) extends BytecodeUtil {
    def this() = this(new ClassBytecodeWriter { })
    def debugLevel = settings.debuginfo.indexOfChoice
    import bytecodeWriter.writeClass

    val MIN_SWITCH_DENSITY = 0.7
    val INNER_CLASSES_FLAGS =
      (ACC_PUBLIC | ACC_PRIVATE | ACC_PROTECTED | ACC_STATIC | ACC_FINAL | ACC_INTERFACE | ACC_ABSTRACT)
    
    val PublicStatic      = ACC_PUBLIC | ACC_STATIC
    val PublicStaticFinal = ACC_PUBLIC | ACC_STATIC | ACC_FINAL

    val StringBuilderClassName = definitions.StringBuilderClass.fullName
    val BoxesRunTime = "scala.runtime.BoxesRunTime"

    val StringBuilderType = new JObjectType(StringBuilderClassName)
    val toStringType      = new JMethodType(JAVA_LANG_STRING, JType.EMPTY_ARRAY)
    val arrayCloneType    = new JMethodType(JAVA_LANG_OBJECT, JType.EMPTY_ARRAY)
    val MethodTypeType    = new JObjectType("java.dyn.MethodType")
    val JavaLangClassType = new JObjectType("java.lang.Class")
    val MethodHandleType  = new JObjectType("java.dyn.MethodHandle")

    // Scala attributes
    val BeanInfoAttr     = definitions.getClass("scala.reflect.BeanInfo")
    val BeanInfoSkipAttr = definitions.getClass("scala.reflect.BeanInfoSkip")
    val BeanDisplayNameAttr = definitions.getClass("scala.reflect.BeanDisplayName")
    val BeanDescriptionAttr = definitions.getClass("scala.reflect.BeanDescription")

    lazy val CloneableClass  = definitions.getClass("java.lang.Cloneable")
    lazy val RemoteInterface = definitions.getClass("java.rmi.Remote")
    lazy val RemoteException = definitions.getClass("java.rmi.RemoteException").tpe

    val versionPickle = {
      val vp = new PickleBuffer(new Array[Byte](16), -1, 0)
      assert(vp.writeIndex == 0, vp)
      vp writeNat PickleFormat.MajorVersion
      vp writeNat PickleFormat.MinorVersion
      vp writeNat 0
      vp
    }

    var clasz: IClass = _
    var method: IMethod = _
    var jclass: JClass = _
    var jmethod: JMethod = _
//    var jcode: JExtendedCode = _

    val fjbgContext = new FJBGContext(49, 0)

    val emitSource = debugLevel >= 1
    val emitLines  = debugLevel >= 2
    val emitVars   = debugLevel >= 3
    
    /** For given symbol return a symbol corresponding to a class that should be declared as inner class.
     * 
     *  For example:
     *  class A {
     *    class B
     *    object C
     *  }
     *  
     *  then method will return NoSymbol for A, the same symbol for A.B (corresponding to A$B class) and A$C$ symbol
     *  for A.C.
     */
    private def innerClassSymbolFor(s: Symbol): Symbol = 
      if (s.isClass) s else if (s.isModule) s.moduleClass else NoSymbol
    
    override def javaName(sym: Symbol): String = {
      /** 
       * Checks if given symbol corresponds to inner class/object and add it to innerClassBuffer 
       *  
       * Note: This method is called recursively thus making sure that we add complete chain
       * of inner class all until root class. 
       */
      def collectInnerClass(s: Symbol): Unit = {
        // TODO: something atPhase(currentRun.flattenPhase.prev) which accounts for
        // being nested in parameterized classes (if we're going to selectively flatten.)
        val x = innerClassSymbolFor(s)
        val isInner = x.isClass && !x.rawowner.isPackageClass
        if (isInner) {
          innerClassBuffer += x
          collectInnerClass(x.rawowner)
        }
      }
      collectInnerClass(sym)
      
      super.javaName(sym)
    }

    /** Write a class to disk, adding the Scala signature (pickled type
     *  information) and inner classes.
     * 
     * @param jclass The FJBG class, where code was emitted
     * @param sym    The corresponding symbol, used for looking up pickled information
     */
    def emitClass(jclass: JClass, sym: Symbol) {
      addInnerClasses(jclass)
      writeClass("" + sym.name, jclass, sym)
    }

    /** Returns the ScalaSignature annotation if it must be added to this class,
     *  none otherwise; furthermore, it adds to `jclass` the ScalaSig marker
     *  attribute (marking that a scala signature annotation is present) or the
     *  Scala marker attribute (marking that the signature for this class is in
     *  another file). The annotation that is returned by this method must be
     *  added to the class' annotations list when generating them.
     *
     *  @param jclass The class file that is being readied.
     *  @param sym    The symbol for which the signature has been entered in
     *                the symData map. This is different than the symbol
     *                that is being generated in the case of a mirror class.
     *  @return       An option that is:
     *                - defined and contains an annotation info of the
     *                  ScalaSignature type, instantiated with the pickle
     *                  signature for sym (a ScalaSig marker attribute has
     *                  been written);
     *                - undefined if the jclass/sym couple must not contain a
     *                  signature (a Scala marker attribute has been written).
     */
    def scalaSignatureAddingMarker(jclass: JClass, sym: Symbol): Option[AnnotationInfo] =
      currentRun.symData get sym match {
        case Some(pickle) if !nme.isModuleName(jclass.getName()) =>
          val scalaAttr =
            fjbgContext.JOtherAttribute(jclass, jclass, tpnme.ScalaSignatureATTR.toString,
                                        versionPickle.bytes, versionPickle.writeIndex)
          jclass addAttribute scalaAttr
          val scalaAnnot = {
            val sigBytes = ScalaSigBytes(pickle.bytes.take(pickle.writeIndex))
            AnnotationInfo(sigBytes.sigAnnot, Nil, List((nme.bytes, sigBytes)))
          }
          pickledBytes += pickle.writeIndex
          currentRun.symData -= sym
          currentRun.symData -= sym.companionSymbol
          Some(scalaAnnot)
        case _ =>
          val markerAttr =
            fjbgContext.JOtherAttribute(jclass, jclass, tpnme.ScalaATTR.toString, new Array[Byte](0), 0)
          jclass addAttribute markerAttr
          None
      }

    var serialVUID: Option[Long] = None
    var isRemoteClass: Boolean = false
    var isParcelableClass = false

    private var innerClassBuffer = mutable.LinkedHashSet[Symbol]()

    def genClass(c: IClass) {
      clasz = c
      innerClassBuffer.clear()

      var parents = c.symbol.info.parents
      var ifaces  = JClass.NO_INTERFACES
      val name    = javaName(c.symbol)
      serialVUID  = None
      isRemoteClass = false
      isParcelableClass = isAndroidParcelableClass(c.symbol)

      if (parents.isEmpty)
        parents = List(ObjectClass.tpe)

      for (annot <- c.symbol.annotations) annot match {
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == SerializableAttr =>
          parents :+= SerializableClass.tpe
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == CloneableAttr =>
          parents :+= CloneableClass.tpe
        case AnnotationInfo(tp, Literal(const) :: _, _) if tp.typeSymbol == SerialVersionUIDAttr =>
          serialVUID = Some(const.longValue)
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == RemoteAttr =>
          parents :+= RemoteInterface.tpe
          isRemoteClass = true
        case _ =>
      }

      parents = parents.distinct

      if (parents.tail.nonEmpty)
        ifaces = mkArray(parents drop 1 map (x => javaName(x.typeSymbol)))

      jclass = fjbgContext.JClass(javaFlags(c.symbol),
                                  name,
                                  javaName(parents(0).typeSymbol),
                                  ifaces,
                                  c.cunit.source.toString)

      if (isStaticModule(c.symbol) || serialVUID != None || isParcelableClass ||
          clasz.bootstrapClass.isDefined) {
        if (isStaticModule(c.symbol))
          addModuleInstanceField
        addStaticInit(jclass, c.lookupStaticCtor)

        if (isTopLevelModule(c.symbol)) {
          if (c.symbol.companionClass == NoSymbol)
            generateMirrorClass(c.symbol, c.cunit.source.toString)
          else
            log("No mirror class for module with linked class: " +
                c.symbol.fullName)
        }
      }
      else {
        c.lookupStaticCtor foreach (constructor => addStaticInit(jclass, Some(constructor)))

        // it must be a top level class (name contains no $s)
        def isCandidateForForwarders(sym: Symbol): Boolean =
          atPhase(currentRun.picklerPhase.next) {
            !(sym.name.toString contains '$') && sym.hasModuleFlag && !sym.isImplClass && !sym.isNestedClass
          }

        // At some point this started throwing lots of exceptions as a compile was finishing.
        // error: java.lang.AssertionError:
        //   assertion failed: List(object package$CompositeThrowable, object package$CompositeThrowable)
        // ...is the one I've seen repeatedly.  Suppressing.
        val lmoc = (
          try c.symbol.companionModule
          catch { case x: AssertionError =>
            Console.println("Suppressing failed assert: " + x)
            NoSymbol
          }
        )
        // add static forwarders if there are no name conflicts; see bugs #363 and #1735
        if (lmoc != NoSymbol && !c.symbol.isInterface) {
          if (isCandidateForForwarders(lmoc) && !settings.noForwarders.value) {
            log("Adding static forwarders from '%s' to implementations in '%s'".format(c.symbol, lmoc))
            addForwarders(jclass, lmoc.moduleClass)
          }
        }
      }

      if (clasz.bootstrapClass.isDefined)
        jclass setBootstrapClass clasz.bootstrapClass.get

      clasz.fields foreach genField
      clasz.methods foreach genMethod

      val ssa = scalaSignatureAddingMarker(jclass, c.symbol)
      addGenericSignature(jclass, c.symbol, c.symbol.owner)
      addAnnotations(jclass, c.symbol.annotations ++ ssa)

      addEnclosingMethodAttribute(jclass, c.symbol)
      emitClass(jclass, c.symbol)
      
      if (c.symbol hasAnnotation BeanInfoAttr)
        genBeanInfoClass(c)
    }

    private def addEnclosingMethodAttribute(jclass: JClass, clazz: Symbol) {
      val sym = clazz.originalEnclosingMethod
      if (sym.isMethod) {
        log("enclosing method for %s is %s (in %s)".format(clazz, sym, sym.enclClass))
        jclass addAttribute fjbgContext.JEnclosingMethodAttribute(
          jclass,
          javaName(sym.enclClass),
          javaName(sym),
          javaType(sym)
        )
      } else if (clazz.isAnonymousClass) {
        val enclClass = clazz.rawowner        
        assert(enclClass.isClass, enclClass)
        val sym = enclClass.primaryConstructor
        if (sym == NoSymbol)
          log("Ran out of room looking for an enclosing method for %s: no constructor here.".format(
            enclClass, clazz)
          )
        else {
          log("enclosing method for %s is %s (in %s)".format(clazz, sym, enclClass))
          jclass addAttribute fjbgContext.JEnclosingMethodAttribute(
            jclass,
            javaName(enclClass),
            javaName(sym),
            javaType(sym).asInstanceOf[JMethodType]
          )
        }
      }
    }

    /**
     * Generate a bean info class that describes the given class.
     *
     * @author Ross Judson (ross.judson@soletta.com)
     */
    def genBeanInfoClass(c: IClass) {
      val description = c.symbol.annotations.find(_.atp.typeSymbol == BeanDescriptionAttr)
      // informProgress(description.toString)

      val beanInfoClass = fjbgContext.JClass(javaFlags(c.symbol),
            javaName(c.symbol) + "BeanInfo",
            "scala/reflect/ScalaBeanInfo",
            JClass.NO_INTERFACES,
            c.cunit.source.toString)

      var fieldList = List[String]()
      for (f <- clasz.fields if f.symbol.hasGetter;
	         val g = f.symbol.getter(c.symbol);
	         val s = f.symbol.setter(c.symbol);
	         if g.isPublic && !(f.symbol.name startsWith "$"))  // inserting $outer breaks the bean
        fieldList = javaName(f.symbol) :: javaName(g) :: (if (s != NoSymbol) javaName(s) else null) :: fieldList
      val methodList = 
	     for (m <- clasz.methods 
	         if !m.symbol.isConstructor &&
	         m.symbol.isPublic &&
	         !(m.symbol.name startsWith "$") &&
	         !m.symbol.isGetter &&
	         !m.symbol.isSetter) yield javaName(m.symbol)

      val constructor = beanInfoClass.addNewMethod(ACC_PUBLIC, "<init>", JType.VOID, new Array[JType](0), new Array[String](0))
      val jcode = constructor.getCode().asInstanceOf[JExtendedCode]
      val strKind = new JObjectType(javaName(StringClass))
      val stringArrayKind = new JArrayType(strKind)
      val conType = new JMethodType(JType.VOID, Array(javaType(ClassClass), stringArrayKind, stringArrayKind))

      def push(lst:Seq[String]) {
        var fi = 0
        for (f <- lst) {
          jcode.emitDUP()
          jcode emitPUSH fi
          if (f != null)
            jcode emitPUSH f
          else
            jcode.emitACONST_NULL()
          jcode emitASTORE strKind
          fi += 1
        } 
      }

      jcode.emitALOAD_0()
      // push the class
      jcode emitPUSH javaType(c.symbol).asInstanceOf[JReferenceType]

      // push the the string array of field information
      jcode emitPUSH fieldList.length
      jcode emitANEWARRAY strKind
      push(fieldList)

      // push the string array of method information	
      jcode emitPUSH methodList.length
      jcode emitANEWARRAY strKind
      push(methodList)

      // invoke the superclass constructor, which will do the
      // necessary java reflection and create Method objects.	
      jcode.emitINVOKESPECIAL("scala/reflect/ScalaBeanInfo", "<init>", conType)
      jcode.emitRETURN()

      // write the bean information class file.
      writeClass("BeanInfo ", beanInfoClass, c.symbol)
    }

    /** Add the given 'throws' attributes to jmethod */
    def addExceptionsAttribute(jmethod: JMethod, excs: List[AnnotationInfo]) {
      if (excs.isEmpty) return

      val cpool = jmethod.getConstantPool
      val buf: ByteBuffer = ByteBuffer.allocate(512)
      var nattr = 0

      // put some random value; the actual number is determined at the end
      buf putShort 0xbaba.toShort

      for (AnnotationInfo(tp, List(exc), _) <- excs.distinct if tp.typeSymbol == ThrowsClass) {
        val Literal(const) = exc
        buf.putShort(
          cpool.addClass(
            javaName(const.typeValue.typeSymbol)).shortValue)
        nattr += 1
      }

      assert(nattr > 0, nattr)
      buf.putShort(0, nattr.toShort)
      addAttribute(jmethod, tpnme.ExceptionsATTR, buf)
    }

    /** Whether an annotation should be emitted as a Java annotation
     *   .initialize: if 'annot' is read from pickle, atp might be un-initialized
     */
    private def shouldEmitAnnotation(annot: AnnotationInfo) =
      annot.atp.typeSymbol.initialize.isJavaDefined &&
      annot.atp.typeSymbol.isNonBottomSubClass(ClassfileAnnotationClass) &&
      annot.args.isEmpty

    private def emitJavaAnnotations(cpool: JConstantPool, buf: ByteBuffer, annotations: List[AnnotationInfo]): Int = {
      def emitArgument(arg: ClassfileAnnotArg): Unit = arg match {
        case LiteralAnnotArg(const) =>
          const.tag match {
            case BooleanTag =>
              buf put 'Z'.toByte
              buf putShort cpool.addInteger(if(const.booleanValue) 1 else 0).toShort
            case ByteTag    =>
              buf put 'B'.toByte
              buf putShort cpool.addInteger(const.byteValue).toShort
            case ShortTag   =>
              buf put 'S'.toByte
              buf putShort cpool.addInteger(const.shortValue).toShort
            case CharTag    =>
              buf put 'C'.toByte
              buf putShort cpool.addInteger(const.charValue).toShort
            case IntTag     =>
              buf put 'I'.toByte
              buf putShort cpool.addInteger(const.intValue).toShort
            case LongTag    =>
              buf put 'J'.toByte
              buf putShort cpool.addLong(const.longValue).toShort
            case FloatTag   =>
              buf put 'F'.toByte
              buf putShort cpool.addFloat(const.floatValue).toShort
            case DoubleTag  =>
              buf put 'D'.toByte
              buf putShort cpool.addDouble(const.doubleValue).toShort
            case StringTag  =>
              buf put 's'.toByte
              buf putShort cpool.addUtf8(const.stringValue).toShort
            case ClassTag   =>
              buf put 'c'.toByte
              buf putShort cpool.addUtf8(javaType(const.typeValue).getSignature()).toShort
            case EnumTag =>
              buf put 'e'.toByte
              buf putShort cpool.addUtf8(javaType(const.tpe).getSignature()).toShort
              buf putShort cpool.addUtf8(const.symbolValue.name.toString).toShort
          }

        case sb@ScalaSigBytes(bytes) if !sb.isLong =>
          buf put 's'.toByte
          buf putShort cpool.addUtf8(sb.encodedBytes).toShort

        case sb@ScalaSigBytes(bytes) if sb.isLong =>
          buf put '['.toByte
          val stringCount = (sb.encodedBytes.length / 65534) + 1
          buf putShort stringCount.toShort
          for (i <- 0 until stringCount) {
            buf put 's'.toByte
            val j = i * 65535
            val string = sb.encodedBytes.slice(j, j + 65535)
            buf putShort cpool.addUtf8(string).toShort
          }

        case ArrayAnnotArg(args) =>
          buf put '['.toByte
          buf putShort args.length.toShort
          args foreach emitArgument

        case NestedAnnotArg(annInfo) =>
          buf put '@'.toByte
          emitAnnotation(annInfo)
      }

      def emitAnnotation(annotInfo: AnnotationInfo) {
        val AnnotationInfo(typ, args, assocs) = annotInfo
        val jtype = javaType(typ)
        buf putShort cpool.addUtf8(jtype.getSignature()).toShort
        assert(args.isEmpty, args)
        buf putShort assocs.length.toShort
        for ((name, value) <- assocs) {
          buf putShort cpool.addUtf8(name.toString).toShort
          emitArgument(value)
        }
      }

      var nannots = 0
      val pos = buf.position()

      // put some random value; the actual number of annotations is determined at the end
      buf putShort 0xbaba.toShort

      for (annot <- annotations if shouldEmitAnnotation(annot)) {
        nannots += 1
        emitAnnotation(annot)
      }

      // save the number of annotations
      buf.putShort(pos, nannots.toShort)
      nannots
    }

    // @M don't generate java generics sigs for (members of) implementation
    // classes, as they are monomorphic (TODO: ok?)
    private def needsGenericSignature(sym: Symbol) = !(
      // PP: This condition used to include sym.hasExpandedName, but this leads
      // to the total loss of generic information if a private member is
      // accessed from a closure: both the field and the accessor were generated
      // without it.  This is particularly bad because the availability of
      // generic information could disappear as a consequence of a seemingly
      // unrelated change.
         sym.isSynthetic
      || sym.isLiftedMethod
      || sym.isBridge
      || (sym.ownerChain exists (_.isImplClass))
    )
    def addGenericSignature(jmember: JMember, sym: Symbol, owner: Symbol) {
      if (needsGenericSignature(sym)) {
        val memberTpe = atPhase(currentRun.erasurePhase)(owner.thisType.memberInfo(sym))
        // println("addGenericSignature sym: " + sym.fullName + " : " + memberTpe + " sym.info: " + sym.info)
        // println("addGenericSignature: "+ (sym.ownerChain map (x => (x.name, x.isImplClass))))
        erasure.javaSig(sym, memberTpe) foreach { sig =>
          debuglog("sig(" + jmember.getName + ", " + sym + ", " + owner + ")      " + sig)
          /** Since we're using a sun internal class for signature validation,
           *  we have to allow for it not existing or otherwise malfunctioning:
           *  in which case we treat every signature as valid.  Medium term we
           *  should certainly write independent signature validation.
           */
          if (settings.Xverify.value && !erasure.isValidSignature(sym, sig)) {
            clasz.cunit.warning(sym.pos, 
                """|compiler bug: created invalid generic signature for %s in %s
                   |signature: %s
                   |if this is reproducible, please report bug at http://lampsvn.epfl.ch/trac/scala
                """.trim.stripMargin.format(sym, sym.owner.skipPackageObject.fullName, sig))
            return
          }
          if ((settings.check.value contains "genjvm")) {
            val normalizedTpe = atPhase(currentRun.erasurePhase)(erasure.prepareSigMap(memberTpe))
            val bytecodeTpe = owner.thisType.memberInfo(sym)
            if (!sym.isType && !sym.isConstructor && !(erasure.erasure(sym, normalizedTpe) =:= bytecodeTpe)) {
              clasz.cunit.warning(sym.pos, 
                  """|compiler bug: created generic signature for %s in %s that does not conform to its erasure
                     |signature: %s
                     |original type: %s
                     |normalized type: %s
                     |erasure type: %s
                     |if this is reproducible, please report bug at http://lampsvn.epfl.ch/trac/scala
                  """.trim.stripMargin.format(sym, sym.owner.skipPackageObject.fullName, sig, memberTpe, normalizedTpe, bytecodeTpe))
               return
            }
          }
          val index = jmember.getConstantPool.addUtf8(sig).toShort
          if (opt.verboseDebug || erasure.traceSignatures)
            atPhase(currentRun.erasurePhase) {
              log("new signature for " + sym+":"+sym.info)
              log("  " + sig)
            }
          val buf = ByteBuffer.allocate(2)
          buf putShort index
          addAttribute(jmember, tpnme.SignatureATTR, buf)
        }
      }
    }

    def addAnnotations(jmember: JMember, annotations: List[AnnotationInfo]) {
      if (annotations.exists(_.atp.typeSymbol == definitions.DeprecatedAttr)) {
        val attr = jmember.getContext().JOtherAttribute(
          jmember.getJClass(), jmember, tpnme.DeprecatedATTR.toString,
          new Array[Byte](0), 0)
        jmember addAttribute attr
      }

      val toEmit = annotations filter shouldEmitAnnotation
      if (toEmit.isEmpty) return

      val buf: ByteBuffer = ByteBuffer.allocate(2048)
      emitJavaAnnotations(jmember.getConstantPool, buf, toEmit)
      addAttribute(jmember, tpnme.RuntimeAnnotationATTR, buf)
    }

    def addParamAnnotations(jmethod: JMethod, pannotss: List[List[AnnotationInfo]]) {
      val annotations = pannotss map (_ filter shouldEmitAnnotation)
      if (annotations forall (_.isEmpty)) return

      val buf: ByteBuffer = ByteBuffer.allocate(2048)

      // number of parameters
      buf.put(annotations.length.toByte)
      for (annots <- annotations)
        emitJavaAnnotations(jmethod.getConstantPool, buf, annots)

      addAttribute(jmethod, tpnme.RuntimeParamAnnotationATTR, buf)
    }

    def addAttribute(jmember: JMember, name: Name, buf: ByteBuffer) {
      if (buf.position() < 2)
        return

      val length = buf.position()
      val arr = buf.array().slice(0, length)

      val attr = jmember.getContext().JOtherAttribute(jmember.getJClass(),
                                                      jmember,
                                                      name.toString,
                                                      arr,
                                                      length)
      jmember addAttribute attr
    }

    def addInnerClasses(jclass: JClass) {
      /** The outer name for this inner class. Note that it returns null
       *  when the inner class should not get an index in the constant pool.
       *  That means non-member classes (anonymous). See Section 4.7.5 in the JVMS.
       */
      def outerName(innerSym: Symbol): String = {
        if (innerSym.originalEnclosingMethod != NoSymbol)
          null
        else {
          val outerName = javaName(innerSym.rawowner)
          if (isTopLevelModule(innerSym.rawowner)) "" + nme.stripModuleSuffix(outerName)
          else outerName
        }
      }

      def innerName(innerSym: Symbol): String = 
        if (innerSym.isAnonymousClass || innerSym.isAnonymousFunction)
          null
        else
          innerSym.rawname + innerSym.moduleSuffix

      // add inner classes which might not have been referenced yet
      atPhase(currentRun.erasurePhase.next) {
        for (sym <- List(clasz.symbol, clasz.symbol.linkedClassOfClass); m <- sym.info.decls.map(innerClassSymbolFor) if m.isClass)
          innerClassBuffer += m
      }

      val allInners = innerClassBuffer.toList
      if (allInners.nonEmpty) {
        log(clasz.symbol.fullName('.') + " contains " + allInners.size + " inner classes.")
        val innerClassesAttr = jclass.getInnerClasses()
        // sort them so inner classes succeed their enclosing class
        // to satisfy the Eclipse Java compiler
        for (innerSym <- allInners sortBy (_.name.length)) {
          val flags = {
            val staticFlag = if (innerSym.rawowner.hasModuleFlag) ACC_STATIC else 0
            (javaFlags(innerSym) | staticFlag) & INNER_CLASSES_FLAGS
          }
          val jname = javaName(innerSym)
          val oname = outerName(innerSym)
          val iname = innerName(innerSym)

          // Mimicking javap inner class output
          debuglog(
            if (oname == null || iname == null) "//class " + jname
            else "//%s=class %s of class %s".format(iname, jname, oname)
          )

          innerClassesAttr.addEntry(jname, oname, iname, flags)
        }
      }
    }

    def genField(f: IField) {
      debuglog("Adding field: " + f.symbol.fullName)

      val attributes = f.symbol.annotations.map(_.atp.typeSymbol).foldLeft(0) {
        case (res, TransientAttr) => res | ACC_TRANSIENT
        case (res, VolatileAttr)  => res | ACC_VOLATILE
        case (res, _)             => res
      }

      var flags = javaFlags(f.symbol)
      if (!f.symbol.isMutable)
        flags |= ACC_FINAL

      val jfield =
        jclass.addNewField(flags | attributes,
                           javaName(f.symbol),
                           javaType(f.symbol.tpe))

      addGenericSignature(jfield, f.symbol, clasz.symbol)
      addAnnotations(jfield, f.symbol.annotations)
    }

    def genMethod(m: IMethod) {
      if (m.symbol.isStaticConstructor) return
      if ((m.symbol.name == nme.getClass_) && m.params.isEmpty) return

      debuglog("Generating method " + m.symbol.fullName)
      method = m
      endPC.clear
      computeLocalVarsIndex(m)

      var resTpe = javaType(m.symbol.tpe.resultType)
      if (m.symbol.isClassConstructor)
        resTpe = JType.VOID

      var flags = javaFlags(m.symbol)
      if (jclass.isInterface)
        flags |= ACC_ABSTRACT

      if (m.symbol.isStrictFP)
        flags |= ACC_STRICT

      // native methods of objects are generated in mirror classes
      if (method.native)
        flags |= ACC_NATIVE

      jmethod = jclass.addNewMethod(flags,
                                    javaName(m.symbol),
                                    resTpe,
                                    mkArray(m.params map (p => javaType(p.kind))),
                                    mkArray(m.params map (p => javaName(p.sym))))

      addRemoteException(jmethod, m.symbol)

      if (!jmethod.isAbstract() && !method.native) {
        val jcode = jmethod.getCode().asInstanceOf[JExtendedCode]

        // add a fake local for debugging purposes
        if (emitVars && isClosureApply(method.symbol)) {
          val outerField = clasz.symbol.info.decl(nme.OUTER_LOCAL)
          if (outerField != NoSymbol) {
            log("Adding fake local to represent outer 'this' for closure " + clasz)
            val _this = new Local(
              method.symbol.newVariable(NoPosition, nme.FAKE_LOCAL_THIS), toTypeKind(outerField.tpe), false)
            m.locals = m.locals ::: List(_this)
            computeLocalVarsIndex(m) // since we added a new local, we need to recompute indexes

            jcode.emitALOAD_0()
            jcode.emitGETFIELD(javaName(clasz.symbol),
                               javaName(outerField),
                               javaType(outerField))
            jcode.emitSTORE(indexOf(_this), javaType(_this.kind))
          }
        }

        for (local <- m.locals if ! m.params.contains(local)) {
          debuglog("add local var: " + local)
          jmethod.addNewLocalVariable(javaType(local.kind), javaName(local.sym))
        }

        genCode(m)
        if (emitVars)
          genLocalVariableTable(m, jcode)
      }

      addGenericSignature(jmethod, m.symbol, clasz.symbol)
      val (excs, others) = splitAnnotations(m.symbol.annotations, ThrowsClass)
      addExceptionsAttribute(jmethod, excs)
      addAnnotations(jmethod, others)
      addParamAnnotations(jmethod, m.params.map(_.sym.annotations))
      
      // check for code size
      try jmethod.freeze()
      catch {
        case e: JCode.CodeSizeTooBigException =>
          clasz.cunit.error(m.symbol.pos, "Code size exceeds JVM limits: %d".format(e.codeSize))
          throw e
      }
    }

    private def addRemoteException(jmethod: JMethod, meth: Symbol) {
      def isRemoteThrows(ainfo: AnnotationInfo) = ainfo match {
        case AnnotationInfo(tp, List(arg), _) if tp.typeSymbol == ThrowsClass =>
          arg match {
            case Literal(Constant(tpe: Type)) if tpe.typeSymbol == RemoteException.typeSymbol => true
            case _ => false
          }
        case _ => false
      }

      if (isRemoteClass ||
          (meth.hasAnnotation(RemoteAttr) && jmethod.isPublic)) {
        val c = Constant(RemoteException)
        val ainfo = AnnotationInfo(ThrowsClass.tpe, List(Literal(c).setType(c.tpe)), List())
        if (!meth.annotations.exists(isRemoteThrows)) {
          meth addAnnotation ainfo
        }      
      }
    }


    /** Return a pair of lists of annotations, first one containing all 
     *  annotations for the given symbol, and the rest.
     */
    private def splitAnnotations(annotations: List[AnnotationInfo], annotSym: Symbol): (List[AnnotationInfo], List[AnnotationInfo]) = {
      annotations.partition { a => a match {
        case AnnotationInfo(tp, _, _) if tp.typeSymbol == annotSym => true
        case _ => false
      }}
    }

    private def isClosureApply(sym: Symbol): Boolean = {
      (sym.name == nme.apply) &&
      sym.owner.isSynthetic &&
      sym.owner.tpe.parents.exists { t => 
        val TypeRef(_, sym, _) = t
        FunctionClass contains sym
      }
    }

    def addModuleInstanceField() {
      jclass.addNewField(PublicStaticFinal,
                        nme.MODULE_INSTANCE_FIELD.toString,
                        jclass.getType())
    }

    def addStaticInit(cls: JClass, mopt: Option[IMethod]) {
      val clinitMethod = cls.addNewMethod(PublicStatic,
                                          "<clinit>",
                                          JType.VOID,
                                          JType.EMPTY_ARRAY,
                                          new Array[String](0))
      val clinit = clinitMethod.getCode().asInstanceOf[JExtendedCode]

      mopt match {
       	case Some(m) =>
          if (clasz.bootstrapClass.isDefined) legacyEmitBootstrapMethodInstall(clinit)

          val oldLastBlock = m.code.blocks.last
          val lastBlock = m.code.newBlock
          oldLastBlock.replaceInstruction(oldLastBlock.length - 1, JUMP(lastBlock))

          if (isStaticModule(clasz.symbol)) {
            // call object's private ctor from static ctor
            lastBlock emit NEW(REFERENCE(m.symbol.enclClass))
            lastBlock emit CALL_METHOD(m.symbol.enclClass.primaryConstructor, Static(true))
          }

          // add serialVUID code
          serialVUID foreach { value =>
            import Flags._, definitions._
            val fieldName = "serialVersionUID"
            val fieldSymbol = clasz.symbol.newValue(NoPosition, newTermName(fieldName))
                                .setFlag(STATIC | FINAL)
                                .setInfo(longType)
            clasz addField new IField(fieldSymbol)
            lastBlock emit CONSTANT(Constant(value))
            lastBlock emit STORE_FIELD(fieldSymbol, true)
          }

          if (isParcelableClass)
            addCreatorCode(BytecodeGenerator.this, lastBlock)

          if (clasz.bootstrapClass.isDefined) {
            // emit bootstrap method install
            //emitBootstrapMethodInstall(block)
          }

          lastBlock emit RETURN(UNIT)
          lastBlock.close

       	  method = m
       	  jmethod = clinitMethod
       	  genCode(m)
       	case None =>
          legacyStaticInitializer(cls, clinit)
      }
    }

    private def legacyStaticInitializer(cls: JClass, clinit: JExtendedCode) {
      if (isStaticModule(clasz.symbol)) {
        clinit emitNEW cls.getName()
        clinit.emitINVOKESPECIAL(cls.getName(),
                                 JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                 JMethodType.ARGLESS_VOID_FUNCTION)
      }

      serialVUID foreach { value =>
        val fieldName = "serialVersionUID"
        jclass.addNewField(PublicStaticFinal, fieldName, JType.LONG)
        clinit emitPUSH value
        clinit.emitPUSH(value)
        clinit.emitPUTSTATIC(jclass.getName(), fieldName, JType.LONG)
      }

      if (isParcelableClass)
        legacyAddCreatorCode(BytecodeGenerator.this, clinit)

      if (clasz.bootstrapClass.isDefined)
        legacyEmitBootstrapMethodInstall(clinit)

      clinit.emitRETURN()
    }

    /** Emit code that installs a boostrap method for invoke dynamic. It
     *  installs the default method, found in scala.runtime.DynamicDispatch.
     */
    def legacyEmitBootstrapMethodInstall(jcode: JExtendedCode) {
      jcode emitPUSH jclass.getType.asInstanceOf[JReferenceType]
      jcode emitPUSH new JObjectType("scala.runtime.DynamicDispatch")
      jcode emitPUSH "bootstrapInvokeDynamic"
      jcode.emitGETSTATIC("java.dyn.Linkage", "BOOTSTRAP_METHOD_TYPE", MethodTypeType)
      jcode.emitDUP
      jcode.emitINVOKESTATIC("scala.Console", "println", new JMethodType(JType.VOID, Array(JAVA_LANG_OBJECT)))
      jcode.emitINVOKESTATIC("java.dyn.MethodHandles", "findStatic", 
                              new JMethodType(MethodHandleType, Array(JavaLangClassType, JAVA_LANG_STRING, MethodTypeType)))
      jcode.emitINVOKESTATIC("java.dyn.Linkage", "registerBootstrapMethod",
                              new JMethodType(JType.VOID, Array(JavaLangClassType, MethodHandleType)))
    }

    /** Add a forwarder for method m */
    def addForwarder(jclass: JClass, module: Symbol, m: Symbol) {
      val moduleName     = javaName(module)
      val methodInfo     = module.thisType.memberInfo(m)
      val paramJavaTypes = methodInfo.paramTypes map javaType
      val paramNames     = 0 until paramJavaTypes.length map ("x_" + _)
      
      /** Forwarders must not be marked final, as the JVM will not allow
       *  redefinition of a final static method, and we don't know what classes
       *  might be subclassing the companion class.  See SI-4827.
       */
      val mirrorMethod = jclass.addNewMethod(
        PublicStatic,
        javaName(m),
        javaType(methodInfo.resultType),
        mkArray(paramJavaTypes),
        mkArray(paramNames))
      val mirrorCode = mirrorMethod.getCode().asInstanceOf[JExtendedCode]
      mirrorCode.emitGETSTATIC(moduleName,
                               nme.MODULE_INSTANCE_FIELD.toString,
                               new JObjectType(moduleName))

      var i = 0
      var index = 0
      var argTypes = mirrorMethod.getArgumentTypes()
      while (i < argTypes.length) {
        mirrorCode.emitLOAD(index, argTypes(i))
        index += argTypes(i).getSize()
        i += 1
      }

      mirrorCode.emitINVOKEVIRTUAL(moduleName, mirrorMethod.getName, javaType(m).asInstanceOf[JMethodType])
      mirrorCode emitRETURN mirrorMethod.getReturnType()

      addRemoteException(mirrorMethod, m)
      // only add generic signature if the method is concrete; bug #1745
      if (!m.isDeferred)
        addGenericSignature(mirrorMethod, m, module)

      val (throws, others) = splitAnnotations(m.annotations, ThrowsClass)
      addExceptionsAttribute(mirrorMethod, throws)
      addAnnotations(mirrorMethod, others)
      addParamAnnotations(mirrorMethod, m.info.params.map(_.annotations))
    } 

    /** Add forwarders for all methods defined in `module` that don't conflict
     *  with methods in the companion class of `module`. A conflict arises when
     *  a method with the same name is defined both in a class and its companion
     *  object: method signature is not taken into account.
     */
    def addForwarders(jclass: JClass, moduleClass: Symbol) {
      assert(moduleClass.isModuleClass, moduleClass)
      debuglog("Dumping mirror class for object: " + moduleClass)

      val className    = jclass.getName
      val linkedClass  = moduleClass.companionClass
      val linkedModule = linkedClass.companionSymbol

      /** There was a bit of a gordian logic knot here regarding forwarders.
       *  All we really have to do is exclude certain categories of symbols and
       *  then all matching names.
       */
      def memberNames(sym: Symbol) = sym.info.members map (_.name.toString) toSet 
      lazy val membersInCommon     = 
        memberNames(linkedModule) intersect memberNames(linkedClass)

      /** Should method `m` get a forwarder in the mirror class? */
      def shouldForward(m: Symbol): Boolean = (
        m.owner != ObjectClass
        && m.isMethod
        && m.isPublic
        && !m.hasFlag(Flags.CASE | Flags.DEFERRED | Flags.SPECIALIZED | Flags.LIFTED)
        && !m.isConstructor
        && !m.isStaticMember
        && !membersInCommon(m.name.toString)
      )

      for (m <- moduleClass.info.nonPrivateMembers) {
        if (shouldForward(m)) {
          log("Adding static forwarder for '%s' from %s to '%s'".format(m, className, moduleClass))
          addForwarder(jclass, moduleClass, m)
        }
        else debuglog("No forwarder for '%s' from %s to '%s'".format(m, className, moduleClass))
      }
    }

    /** Generate a mirror class for a top-level module. A mirror class is a class
     *  containing only static methods that forward to the corresponding method
     *  on the MODULE instance of the given Scala object.  It will only be
     *  generated if there is no companion class: if there is, an attempt will
     *  instead be made to add the forwarder methods to the companion class.
     */
    def generateMirrorClass(clasz: Symbol, sourceFile: String) {
      import JAccessFlags._
      val moduleName = javaName(clasz) // + "$"
      val mirrorName = moduleName.substring(0, moduleName.length() - 1)
      val mirrorClass = fjbgContext.JClass(ACC_SUPER | ACC_PUBLIC | ACC_FINAL,
                                           mirrorName,
                                           JAVA_LANG_OBJECT.getName,
                                           JClass.NO_INTERFACES,
                                           sourceFile)

      log("Dumping mirror class for '%s'".format(mirrorClass.getName))
      addForwarders(mirrorClass, clasz)
      val ssa = scalaSignatureAddingMarker(mirrorClass, clasz.companionSymbol)
      addAnnotations(mirrorClass, clasz.annotations ++ ssa)
      emitClass(mirrorClass, clasz)
    }

    var linearization: List[BasicBlock] = Nil
    var isModuleInitialized = false

    /**
     *  @param m ...
     */
    def genCode(m: IMethod) {
      val jcode = jmethod.getCode.asInstanceOf[JExtendedCode]

      def makeLabels(bs: List[BasicBlock]) = {
        debuglog("Making labels for: " + method)

        mutable.HashMap(bs map (_ -> jcode.newLabel) : _*)
      }

      isModuleInitialized = false

      linearization = linearizer.linearize(m)
      val labels = makeLabels(linearization)
      /** local variables whose scope appears in this block. */
      val varsInBlock: mutable.Set[Local] = new mutable.HashSet

      var nextBlock: BasicBlock = linearization.head

      def genBlocks(l: List[BasicBlock]): Unit = l match {
        case Nil => ()
        case x :: Nil => nextBlock = null; genBlock(x)
        case x :: y :: ys => nextBlock = y; genBlock(x); genBlocks(y :: ys)
      }

    /** Generate exception handlers for the current method. */
    def genExceptionHandlers() {

      /** Return a list of pairs of intervals where the handler is active.
       *  The intervals in the list have to be inclusive in the beginning and
       *  exclusive in the end: [start, end).
       */
      def ranges(e: ExceptionHandler): List[(Int, Int)] = {
        var covered = e.covered
        var ranges: List[(Int, Int)] = Nil
        var start = -1
        var end = -1

        linearization foreach { b =>
          if (! (covered contains b) ) {
            if (start >= 0) { // we're inside a handler range
              end = labels(b).getAnchor()
              ranges ::= ((start, end))
              start = -1
            }
          } else {
            if (start < 0)  // we're not inside a handler range
              start = labels(b).getAnchor()

            end = endPC(b)
            covered -= b
          }
        }

        /* Add the last interval. Note that since the intervals are 
         * open-ended to the right, we have to give a number past the actual
         * code!
         */
        if (start >= 0) {
          ranges ::= ((start, jcode.getPC()))
        }

        if (!covered.isEmpty)
          debuglog("Some covered blocks were not found in method: " + method + 
                " covered: " + covered + " not in " + linearization)
        ranges
      }
      
      for (e <- this.method.exh ; p <- ranges(e).sortBy(_._1)) {
        if (p._1 < p._2) {
          debuglog("Adding exception handler " + e + "at block: " + e.startBlock + " for " + method + 
                " from: " + p._1 + " to: " + p._2 + " catching: " + e.cls);
          val cls = if (e.cls == NoSymbol || e.cls == ThrowableClass) null
                    else javaName(e.cls)
          jcode.addExceptionHandler(p._1, p._2,
                                    labels(e.startBlock).getAnchor(),
                                    cls)
        } else 
          log("Empty exception range: " + p)
      }
    }

    def genBlock(b: BasicBlock) {
      labels(b).anchorToNext()

      debuglog("Generating code for block: " + b + " at pc: " + labels(b).getAnchor())
      var lastMappedPC = 0
      var lastLineNr = 0
      var crtPC = 0
      varsInBlock.clear()

      for (instr <- b) {

        instr match {
          case THIS(clasz) =>
            jcode.emitALOAD_0()

          case CONSTANT(const) =>
            genConstant(jcode, const)

          case LOAD_ARRAY_ITEM(kind) =>
            jcode.emitALOAD(javaType(kind))

          case LOAD_LOCAL(local) =>
            jcode.emitLOAD(indexOf(local), javaType(local.kind))

          case lf @ LOAD_FIELD(field, isStatic) =>
            var owner = javaName(lf.hostClass)
            debuglog("LOAD_FIELD with owner: " + owner +
                  " flags: " + Flags.flagsToString(field.owner.flags))
            if (isStatic)
              jcode.emitGETSTATIC(owner,
                                  javaName(field),
                                  javaType(field))
            else
              jcode.emitGETFIELD(owner,
                                  javaName(field),
                                  javaType(field))

          case LOAD_MODULE(module) =>
//            assert(module.isModule, "Expected module: " + module)
            debuglog("generating LOAD_MODULE for: " + module + " flags: " +
                  Flags.flagsToString(module.flags));
            if (clasz.symbol == module.moduleClass && jmethod.getName() != nme.readResolve.toString)
              jcode.emitALOAD_0()
            else
              jcode.emitGETSTATIC(javaName(module) /* + "$" */ ,
                                  nme.MODULE_INSTANCE_FIELD.toString,
                                  javaType(module))

          case STORE_ARRAY_ITEM(kind) =>
            jcode emitASTORE javaType(kind)

          case STORE_LOCAL(local) =>
            jcode.emitSTORE(indexOf(local), javaType(local.kind))

          case STORE_THIS(_) =>
            // this only works for impl classes because the self parameter comes first
            // in the method signature. If that changes, this code has to be revisited.
            jcode.emitASTORE_0()

          case STORE_FIELD(field, isStatic) =>
            val owner = javaName(field.owner)
            if (isStatic)
              jcode.emitPUTSTATIC(owner,
                                  javaName(field),
                                  javaType(field))
            else
              jcode.emitPUTFIELD(owner,
                                  javaName(field),
                                  javaType(field))

          case CALL_PRIMITIVE(primitive) =>
            genPrimitive(primitive, instr.pos)

          /** Special handling to access native Array.clone() */
          case call @ CALL_METHOD(definitions.Array_clone, Dynamic) =>
            val target: String = javaType(call.targetTypeKind).getSignature()
            jcode.emitINVOKEVIRTUAL(target, "clone", arrayCloneType)

          case call @ CALL_METHOD(method, style) =>
            val owner: String = javaName(method.owner)
            // reference the type of the receiver instead of the method owner (if not an interface!)
            val dynamicOwner =
              if (needsInterfaceCall(call.hostClass)) owner
              else javaName(call.hostClass)
            val jname = javaName(method)
            val jtype = javaType(method).asInstanceOf[JMethodType]

            style match {
              case InvokeDynamic =>
                jcode.emitINVOKEINTERFACE("java.dyn.Dynamic", jname, jtype)

              case Dynamic =>
                if (needsInterfaceCall(method.owner))
                  jcode.emitINVOKEINTERFACE(owner, jname, jtype)
                else
                  jcode.emitINVOKEVIRTUAL(dynamicOwner, jname, jtype)

              case Static(instance) =>
                if (instance)
                  jcode.emitINVOKESPECIAL(owner, jname, jtype)
                else
                  jcode.emitINVOKESTATIC(owner, jname, jtype)

              case SuperCall(_) =>
                jcode.emitINVOKESPECIAL(owner, jname, jtype)
                // we initialize the MODULE$ field immediately after the super ctor
                if (isStaticModule(clasz.symbol) && !isModuleInitialized &&
                    jmethod.getName() == JMethod.INSTANCE_CONSTRUCTOR_NAME &&
                    jname == JMethod.INSTANCE_CONSTRUCTOR_NAME) {
                  isModuleInitialized = true
                  jcode.emitALOAD_0()
                  jcode.emitPUTSTATIC(jclass.getName(),
                                      nme.MODULE_INSTANCE_FIELD.toString,
                                      jclass.getType())
                }
            }

          case BOX(kind) =>
            val boxedType = definitions.boxedClass(kind.toType.typeSymbol)
            val mtype = new JMethodType(javaType(boxedType), Array(javaType(kind)))
            jcode.emitINVOKESTATIC(BoxesRunTime, "boxTo" + boxedType.decodedName, mtype)

          case UNBOX(kind) =>
            val mtype = new JMethodType(javaType(kind), Array(JAVA_LANG_OBJECT))
            jcode.emitINVOKESTATIC(BoxesRunTime, "unboxTo" + kind.toType.typeSymbol.decodedName, mtype)

          case NEW(REFERENCE(cls)) =>
            val className = javaName(cls)
            jcode emitNEW className

          case CREATE_ARRAY(elem, 1) => elem match {
            case REFERENCE(_) | ARRAY(_) =>
              jcode emitANEWARRAY javaType(elem).asInstanceOf[JReferenceType]
            case _ =>
              jcode emitNEWARRAY javaType(elem)
          }

          case CREATE_ARRAY(elem, dims) => 
            jcode.emitMULTIANEWARRAY(javaType(ArrayN(elem, dims)).asInstanceOf[JReferenceType], dims)

          case IS_INSTANCE(tpe) =>
            tpe match {
              case REFERENCE(cls) =>
                jcode emitINSTANCEOF new JObjectType(javaName(cls))
              case ARRAY(elem) =>
                jcode emitINSTANCEOF new JArrayType(javaType(elem))
              case _ =>
                abort("Unknown reference type in IS_INSTANCE: " + tpe)
            }

          case CHECK_CAST(tpe) =>
            tpe match {
              case REFERENCE(cls) =>
                // No need to checkcast for Objects
                if (cls != ObjectClass)
                  jcode emitCHECKCAST new JObjectType(javaName(cls))
              case ARRAY(elem) =>
                jcode emitCHECKCAST new JArrayType(javaType(elem))
              case _ =>
                abort("Unknown reference type in IS_INSTANCE: " + tpe)
            }

          case SWITCH(tags, branches) =>
            val tagArray = new Array[Array[Int]](tags.length)
            var caze = tags
            var i = 0

            while (i < tagArray.length) {
              tagArray(i) = new Array[Int](caze.head.length)
              caze.head.copyToArray(tagArray(i), 0)
              i += 1
              caze = caze.tail
            }
            val branchArray = jcode.newLabels(tagArray.length)
            i = 0
            while (i < branchArray.length) {
              branchArray(i) = labels(branches(i))
              i += 1
            }
            debuglog("Emitting SWITCH:\ntags: " + tags + "\nbranches: " + branches)
            jcode.emitSWITCH(tagArray, 
                             branchArray,
                             labels(branches.last),
                             MIN_SWITCH_DENSITY)
            ()

          case JUMP(whereto) =>
            if (nextBlock != whereto)
              jcode.emitGOTO_maybe_W(labels(whereto), false) // default to short jumps

          case CJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                if (nextBlock == success) {
                  jcode.emitIF_ICMP(conds(negate(cond)), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ICMP(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }

              case REFERENCE(_) | ARRAY(_) =>
                if (nextBlock == success) {
                  jcode.emitIF_ACMP(conds(negate(cond)), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ACMP(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }

              case _ =>
                (kind: @unchecked) match {
                  case LONG   => jcode.emitLCMP()
                  case FLOAT  => 
                    if (cond == LT || cond == LE) jcode.emitFCMPG()
                    else jcode.emitFCMPL()
                  case DOUBLE => 
                    if (cond == LT || cond == LE) jcode.emitDCMPG()
                    else jcode.emitDCMPL()
                }
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure))
                  // .. and fall through to success label
                } else {
                  jcode.emitIF(conds(cond), labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }
            }

          case CZJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure))
                } else {
                  jcode.emitIF(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }

              case REFERENCE(_) | ARRAY(_) =>
                val Success = success
                val Failure = failure
                (cond, nextBlock) match {
                  case (EQ, Success) =>
                    jcode emitIFNONNULL labels(failure)
                  case (NE, Failure) =>
                    jcode emitIFNONNULL labels(success)
                  case (EQ, Failure) =>
                    jcode emitIFNULL labels(success)
                  case (NE, Success) =>
                    jcode emitIFNULL labels(failure)
                  case (EQ, _) =>
                    jcode emitIFNULL labels(success)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                  case (NE, _) =>
                    jcode emitIFNONNULL labels(success)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }

              case _ =>
                (kind: @unchecked) match {
                  case LONG   =>
                    jcode.emitLCONST_0(); jcode.emitLCMP()
                  case FLOAT  => 
                    jcode.emitFCONST_0()
                    if (cond == LT || cond == LE) jcode.emitFCMPG()
                    else jcode.emitFCMPL()                    
                  case DOUBLE => 
                    jcode.emitDCONST_0()
                    if (cond == LT || cond == LE) jcode.emitDCMPG()
                    else jcode.emitDCMPL()
                }
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure))
                } else {
                  jcode.emitIF(conds(cond), labels(success))
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false)
                }
            }

          case RETURN(kind) =>
            jcode emitRETURN javaType(kind)

          case THROW(_) =>
            jcode.emitATHROW()

          case DROP(kind) =>
            kind match {
              case LONG | DOUBLE => jcode.emitPOP2()
              case _ => jcode.emitPOP()
            }

          case DUP(kind) =>
            kind match {
              case LONG | DOUBLE => jcode.emitDUP2()
              case _ => jcode.emitDUP()
            }

          case MONITOR_ENTER() =>
            jcode.emitMONITORENTER()

          case MONITOR_EXIT() =>
            jcode.emitMONITOREXIT()            

          case SCOPE_ENTER(lv) => 
            varsInBlock += lv
            lv.start = jcode.getPC()

          case SCOPE_EXIT(lv) =>
            if (varsInBlock(lv)) {
              lv.ranges = (lv.start, jcode.getPC()) :: lv.ranges
              varsInBlock -= lv
            } 
            else if (b.varsInScope(lv)) {
              lv.ranges = (labels(b).getAnchor(), jcode.getPC()) :: lv.ranges
              b.varsInScope -= lv
            } 
            else dumpMethodAndAbort(method, "Illegal local var nesting")

          case LOAD_EXCEPTION(_) =>
            ()
        }

        crtPC = jcode.getPC()
        
//        assert(instr.pos.source.isEmpty || instr.pos.source.get == (clasz.cunit.source), "sources don't match")
//        val crtLine = instr.pos.line.get(lastLineNr);

        val crtLine = try {
          if (instr.pos == NoPosition) lastLineNr else (instr.pos).line // check NoPosition to avoid costly exception
        } catch {
          case _: UnsupportedOperationException =>
            log("Warning: wrong position in: " + method)
            lastLineNr
        }

        if (b.lastInstruction == instr)
          endPC(b) = jcode.getPC()

        //System.err.println("CRTLINE: " + instr.pos + " " + 
        //           /* (if (instr.pos < clasz.cunit.source.content.length) clasz.cunit.source.content(instr.pos) else '*') + */ " " + crtLine);

        if (crtPC > lastMappedPC) {
          jcode.completeLineNumber(lastMappedPC, crtPC, crtLine)
          lastMappedPC = crtPC
          lastLineNr   = crtLine
        }
      }
      
      // local vars that survived this basic block 
      for (lv <- varsInBlock) {
        lv.ranges = (lv.start, jcode.getPC()) :: lv.ranges
      }
      for (lv <- b.varsInScope) {
        lv.ranges = (labels(b).getAnchor(), jcode.getPC()) :: lv.ranges
      }
    }


    /**
     *  @param primitive ...
     *  @param pos       ...
     */
    def genPrimitive(primitive: Primitive, pos: Position) {
      primitive match {
        case Negation(kind) =>
          kind match {
            case BOOL | BYTE | CHAR | SHORT | INT => 
              jcode.emitINEG()
            case LONG   => jcode.emitLNEG()
            case FLOAT  => jcode.emitFNEG()
            case DOUBLE => jcode.emitDNEG()
            case _ => abort("Impossible to negate a " + kind)
          }

        case Arithmetic(op, kind) =>
          op match {
            case ADD => jcode.emitADD(javaType(kind))
            case SUB =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitISUB()
                case LONG   => jcode.emitLSUB()
                case FLOAT  => jcode.emitFSUB()
                case DOUBLE => jcode.emitDSUB()
              }

            case MUL =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIMUL()
                case LONG   => jcode.emitLMUL()
                case FLOAT  => jcode.emitFMUL()
                case DOUBLE => jcode.emitDMUL()
              }

            case DIV =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIDIV()
                case LONG   => jcode.emitLDIV()
                case FLOAT  => jcode.emitFDIV()
                case DOUBLE => jcode.emitDDIV()
              }

            case REM =>
              (kind: @unchecked) match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIREM()
                case LONG   => jcode.emitLREM()
                case FLOAT  => jcode.emitFREM()
                case DOUBLE => jcode.emitDREM()
              }

            case NOT =>
              kind match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitPUSH(-1)
                  jcode.emitIXOR()
                case LONG   =>
                  jcode.emitPUSH(-1l)
                  jcode.emitLXOR()
                case _ =>
                  abort("Impossible to negate an " + kind)
              }

            case _ =>
              abort("Unknown arithmetic primitive " + primitive)
          }

        case Logical(op, kind) => (op, kind) match {
          case (AND, LONG) =>
            jcode.emitLAND()
          case (AND, INT) =>
            jcode.emitIAND()
          case (AND, _) =>
            jcode.emitIAND()
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));

          case (OR, LONG) =>
            jcode.emitLOR()
          case (OR, INT) =>
            jcode.emitIOR()
          case (OR, _) =>
            jcode.emitIOR()
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));

          case (XOR, LONG) =>
            jcode.emitLXOR()
          case (XOR, INT) =>
            jcode.emitIXOR()
          case (XOR, _) =>
            jcode.emitIXOR()
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));
        }

        case Shift(op, kind) => (op, kind) match {
          case (LSL, LONG) =>
            jcode.emitLSHL()
          case (LSL, INT) =>
            jcode.emitISHL()
          case (LSL, _) =>
            jcode.emitISHL()
            jcode.emitT2T(javaType(INT), javaType(kind))

          case (ASR, LONG) =>
            jcode.emitLSHR()
          case (ASR, INT) =>
            jcode.emitISHR()
          case (ASR, _) =>
            jcode.emitISHR()
            jcode.emitT2T(javaType(INT), javaType(kind))

          case (LSR, LONG) =>
            jcode.emitLUSHR()
          case (LSR, INT) =>
            jcode.emitIUSHR()
          case (LSR, _) =>
            jcode.emitIUSHR()
            jcode.emitT2T(javaType(INT), javaType(kind))
        }

        case Comparison(op, kind) => ((op, kind): @unchecked) match {
          case (CMP, LONG)    => jcode.emitLCMP()
          case (CMPL, FLOAT)  => jcode.emitFCMPL()
          case (CMPG, FLOAT)  => jcode.emitFCMPG()
          case (CMPL, DOUBLE) => jcode.emitDCMPL()
          case (CMPG, DOUBLE) => jcode.emitDCMPL()
        }

        case Conversion(src, dst) =>
          debuglog("Converting from: " + src + " to: " + dst)
          if (dst == BOOL) {
            println("Illegal conversion at: " + clasz +
                    " at: " + pos.source + ":" + pos.line)
          } else
            jcode.emitT2T(javaType(src), javaType(dst))

        case ArrayLength(_) =>
          jcode.emitARRAYLENGTH()

        case StartConcat =>
          jcode emitNEW StringBuilderClassName
          jcode.emitDUP()
          jcode.emitINVOKESPECIAL(StringBuilderClassName,
                                  JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                  JMethodType.ARGLESS_VOID_FUNCTION)

        case StringConcat(el) =>
          val jtype = el match {
            case REFERENCE(_) | ARRAY(_) => JAVA_LANG_OBJECT
            case _ => javaType(el)
          }
          jcode.emitINVOKEVIRTUAL(StringBuilderClassName,
                                  "append",
                                  new JMethodType(StringBuilderType,
                                  Array(jtype)))
        case EndConcat =>
          jcode.emitINVOKEVIRTUAL(StringBuilderClassName,
                                  "toString",
                                  toStringType)

        case _ =>
          abort("Unimplemented primitive " + primitive)
      }
    }

      // genCode starts here
      genBlocks(linearization)

      if (this.method.exh != Nil)
        genExceptionHandlers;
    }


    /** Emit a Local variable table for debugging purposes.
     *  Synthetic locals are skipped. All variables are method-scoped.
     */
    private def genLocalVariableTable(m: IMethod, jcode: JCode) {
      val vars = m.locals filterNot (_.sym.isSynthetic)
      if (vars.isEmpty) return

      val pool = jclass.getConstantPool
      val pc = jcode.getPC()
      var anonCounter = 0
      var entries = 0
      vars.foreach { lv =>
        lv.ranges = mergeEntries(lv.ranges.reverse);
        entries += lv.ranges.length
      }
      if (!jmethod.isStatic()) entries += 1

      val lvTab = ByteBuffer.allocate(2 + 10 * entries)
      def emitEntry(name: String, signature: String, idx: Short, start: Short, end: Short) {
        lvTab putShort start
        lvTab putShort end
        lvTab putShort pool.addUtf8(name).toShort
        lvTab putShort pool.addUtf8(signature).toShort
        lvTab putShort idx
      }

      lvTab.putShort(entries.toShort)

      if (!jmethod.isStatic()) {
        emitEntry("this", jclass.getType().getSignature(), 0, 0.toShort, pc.toShort)
      }

      for (lv <- vars) {
        val name = if (javaName(lv.sym) eq null) {
          anonCounter += 1
          "<anon" + anonCounter + ">"
        } else javaName(lv.sym)

        val index = indexOf(lv).toShort
        val tpe   = javaType(lv.kind).getSignature()
        for ((start, end) <- lv.ranges) {
          emitEntry(name, tpe, index, start.toShort, (end - start).toShort)
        }
      }
      val attr =
        fjbgContext.JOtherAttribute(jclass,
                                    jcode,
                                    tpnme.LocalVariableTableATTR.toString,
                                    lvTab.array())
      jcode addAttribute attr
    }


    /** For each basic block, the first PC address following it. */
    val endPC = new mutable.HashMap[BasicBlock, Int]

    ////////////////////// local vars ///////////////////////

    def sizeOf(sym: Symbol): Int = sizeOf(toTypeKind(sym.tpe))

    def sizeOf(k: TypeKind): Int = k match {
      case DOUBLE | LONG => 2
      case _ => 1
    }

    def indexOf(m: IMethod, sym: Symbol): Int = {
      val Some(local) = m lookupLocal sym
      indexOf(local)
    }

    def indexOf(local: Local): Int = {
      assert(local.index >= 0, "Invalid index for: " + local + "{" + local.## + "}: ")
      local.index
    }

    /**
     * Compute the indexes of each local variable of the given
     * method. *Does not assume the parameters come first!*
     */
    def computeLocalVarsIndex(m: IMethod) {
      var idx = 1
      if (m.symbol.isStaticMember)
        idx = 0;

      for (l <- m.params) {
        debuglog("Index value for " + l + "{" + l.## + "}: " + idx)
        l.index = idx
        idx += sizeOf(l.kind)
      }

      for (l <- m.locals if !(m.params contains l)) {
        debuglog("Index value for " + l + "{" + l.## + "}: " + idx)
        l.index = idx
        idx += sizeOf(l.kind)
      }
    }

    ////////////////////// Utilities ////////////////////////

    /** Calls to methods in 'sym' need invokeinterface? */
    def needsInterfaceCall(sym: Symbol): Boolean = {
      debuglog("checking for interface call: " + sym.fullName)
      // the following call to 'info' may cause certain symbols to fail loading
      // because we're too late in the compilation chain (aliases to overloaded
      // symbols will not be properly resolved, see scala.Range, method
      // `super$++` that fails in UnPickler at LazyTypeRefAndAlias.complete
      if (sym.isTrait) sym.info // needed so that the type is up to date
                                // (erasure may add lateINTERFACE to traits)

      sym.isInterface ||
      (sym.isJavaDefined && sym.isNonBottomSubClass(ClassfileAnnotationClass))
    }

    /** Merge adjacent ranges. */
    private def mergeEntries(ranges: List[(Int, Int)]): List[(Int, Int)] = 
      (ranges.foldLeft(Nil: List[(Int, Int)]) { (collapsed: List[(Int, Int)], p: (Int, Int)) => (collapsed, p) match {
        case (Nil, _) => List(p)
        case ((s1, e1) :: rest, (s2, e2)) if (e1 == s2) => (s1, e2) :: rest
        case _ => p :: collapsed
      }}).reverse
  }

  /**
   * Return the Java modifiers for the given symbol.
   * Java modifiers for classes:
   *  - public, abstract, final, strictfp (not used)
   * for interfaces:
   *  - the same as for classes, without 'final'
   * for fields:
   *  - public, private (*)
   *  - static, final 
   * for methods:
   *  - the same as for fields, plus:
   *  - abstract, synchronized (not used), strictfp (not used), native (not used)
   * 
   *  (*) protected cannot be used, since inner classes 'see' protected members,
   *      and they would fail verification after lifted.
   */
  def javaFlags(sym: Symbol): Int = {
    def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)
    // constructors of module classes should be private
    // PP: why are they only being marked private at this stage and not earlier?
    val privateFlag =
      sym.isPrivate || (sym.isPrimaryConstructor && isTopLevelModule(sym.owner))

    // This does not check .isFinal (which checks flags for the FINAL flag),
    // instead checking rawflags for that flag so as to exclude symbols which
    // received lateFINAL.  These symbols are eligible for inlining, but to
    // avoid breaking proxy software which depends on subclassing, we avoid
    // insisting on their finality in the bytecode.
    val finalFlag = (
         ((sym.rawflags & (Flags.FINAL | Flags.MODULE)) != 0)
      && !sym.enclClass.isInterface
      && !sym.isClassConstructor
    )

    mkFlags(
      if (privateFlag) ACC_PRIVATE else ACC_PUBLIC,
      if (sym.isDeferred || sym.hasAbstractFlag) ACC_ABSTRACT else 0,
      if (sym.isInterface) ACC_INTERFACE else 0,
      if (finalFlag) ACC_FINAL else 0,
      if (sym.isStaticMember) ACC_STATIC else 0,
      if (sym.isBridge || sym.hasFlag(Flags.MIXEDIN) && sym.isMethod) ACC_BRIDGE else 0,
      if (sym.isClass && !sym.isInterface) ACC_SUPER else 0,
      if (sym.isVarargsMethod) ACC_VARARGS else 0
    )
  }

  def isTopLevelModule(sym: Symbol): Boolean =
    atPhase (currentRun.picklerPhase.next) {
      sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
    }

  def isStaticModule(sym: Symbol): Boolean = {
    sym.isModuleClass && !sym.isImplClass && !sym.isLifted
  }

}
