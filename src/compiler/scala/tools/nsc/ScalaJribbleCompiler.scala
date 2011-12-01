package scala.tools.nsc

import com.google.jribble.unit._
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.util.JavaClassPath
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io.AbstractFile
import scala.collection.JavaConversions._

class ScalaJribbleCompiler extends JribbleCompiler {

  private val input = new InMemoryPath("(memory)")
  private val output = new VirtualDirectory("(memory)", None)
  private val settingArgs = List(
    "-g:notailcalls",
    "-Xplugin-classes:scala.tools.factorymanifests.FactoryManifestsPlugin")
  private val files = scala.collection.mutable.Buffer[AbstractFile]()

  override def addClassBytes(internalName: String, classBytes: Array[Byte]) {
    input.addClassBytes(internalName, classBytes);
  }

  override def addJavaSource(internalName: String, content: String) {
    files += input.addJavaSource(internalName, content)
  }

  override def addSource(internalName: String, content: String) {
    files += input.addSource(internalName, content)
  }

  /** Invokes the compiler and returns the new units. */
  def compile(): java.util.Collection[JribbleUnitResult] = {
    val ss = new Settings(error);
    // write to our in-memory directory
    ss.outputDirs setSingleOutput output
    val command = new CompilerCommand(settingArgs, ss);
    // make a new compiler, but with our input as the classpath
    val compiler = new Global(command.settings, null) { self =>
      override lazy val platform: ThisPlatform = {
        new { val global: self.type = self } with backend.JribblePlatform {
          override lazy val classPath = {
            new JavaClassPath(List(input).toIndexedSeq, ClassPath.DefaultJavaContext)
          }
        }
      }
    }
    val run = new compiler.Run();
    run.compileFiles(files.toList)
    // map to group by compilation unit
    val units = scala.collection.mutable.HashMap[String, JribbleUnitResult]()
    // find all .class files by internal name
    val classes = new scala.collection.mutable.HashMap[String, AbstractFile]()
    find(classes, output, "class")
    // find all errors by internal name
    val errors = new scala.collection.mutable.HashMap[String, String]()
    classes foreach { case (internalName, classFile) =>
      println("On " + internalName)
      val sourceFileName = compiler.genJVM.internalNameToSourceName.getOrElse(internalName, sys.error("source file not found for " + internalName))
      println("From " + sourceFileName)
      val jribbleAst = compiler.genJribble.jribbleTypes.getOrElse(internalName, sys.error("jribble not found for " + internalName))
      val unit = units.getOrElseUpdate(sourceFileName, new JribbleUnitResult(internalName))
      val cr = new JribbleClassResult(internalName, classFile.toByteArray, jribbleAst)
      unit.classes.add(cr)
    }
    units.values
  }
  
  private def find(m: scala.collection.mutable.Map[String, AbstractFile], d: AbstractFile, ext: String): Unit = {
    d foreach { f =>
      if (f.isDirectory) {
        find(m, f, ext)
      } else if (f hasExtension ext) {
        m += f.path -> f
      }
    }
  }
  
}

class InMemoryPath(override val name: String) extends ClassPath[AbstractFile] {
  override val asURLs = List()
  override val asClasspathString = null
  override val context = ClassPath.DefaultJavaContext
  // mutable collections so we can add as needed
  override val classes = new scala.collection.mutable.ArrayBuffer[AnyClassRep]()
  override val packages = new scala.collection.mutable.ArrayBuffer[InMemoryPath]()
  override val sourcepaths: IndexedSeq[AbstractFile] = IndexedSeq()

  def addClassBytes(internalName: String, classBytes: Array[Byte]): AbstractFile = {
    val parts = internalName.split('/')
    val f = new io.VirtualFile(parts.last + ".class", parts.dropRight(1).mkString("/"))
    f.output
    getPackage(parts.dropRight(1)).classes += new ClassRep(Some(f), None)
    f
  }

  def addJavaSource(internalName: String, content: String): AbstractFile = {
    val parts = internalName.split('/')
    val f = new io.VirtualFile(parts.last + ".java", parts.dropRight(1).mkString("/"))
    getPackage(parts.dropRight(1)).classes += new ClassRep(None, Some(f))
    f
  }

  def addSource(internalName: String, content: String): AbstractFile = {
    val parts = internalName.split('/')
    val f = new io.VirtualFile(parts.last + ".scala", parts.dropRight(1).mkString("/"))
    getPackage(parts.dropRight(1)).classes += new ClassRep(None, Some(f))
    f
  }

  private def getPackage(parts: Array[String]): InMemoryPath = {
    if (parts.length == 0) {
      return this
    }
    val p = packages.find(_.name == parts(0)) match {
      case Some(p) => p
      case None =>
        val p = new InMemoryPath(parts(0))
        packages += p
        p
    }
    p.getPackage(parts.drop(1))
  }

}

object ScalaJribbleCompiler {
  val primitives = List("Boolean",
    "Byte",
    "Char",
    "Double",
    "Float",
    "Int",
    "Long",
    "Short",
    "Unit").map("scala/" + _)
}