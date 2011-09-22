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

  override def addClassBytes(internalName: String, classBytes: Array[Byte]) {
    input.addClassBytes(internalName, classBytes);
  }

  override def addJavaSource(internalName: String, content: String) {
    input.addJavaSource(internalName, content)
  }

  override def addSource(internalName: String, content: String) {
    input.addSource(internalName, content)
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
    // find all .jribble files by internal name
    val jribble = new scala.collection.mutable.HashMap[String, AbstractFile]()
    find(jribble, output, "jribble")
    // find all .class files by internal name
    val classes = new scala.collection.mutable.HashMap[String, AbstractFile]()
    find(classes, output, "class")
    // find all errors by internal name
    val errors = new scala.collection.mutable.HashMap[String, String]()
    jribble map { case (internalName, jribble) =>
      // for now don't group by compilation unit
      val result = new JribbleUnitResult(internalName)
      val bytes = classes.getOrElse(internalName, sys.error(".class not found " + internalName))
      val cr = new JribbleClassResult(internalName, read(bytes.input), new String(read(jribble.input)))
      result.classes.add(cr)
      result
    }
  }

  // hacky utility method to read InputStream -> Array[Byte]
  private def read(in: java.io.InputStream): Array[Byte] = {
    val baos = new java.io.ByteArrayOutputStream
    var buffer = new Array[Byte](1024)
    var read = in.read(buffer, 0, buffer.length)
    while (read != -1) {
      baos.write(buffer, 0, read)
      read = in.read(buffer, 0, buffer.length)
    }
    baos.toByteArray
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

  def addClassBytes(internalName: String, classBytes: Array[Byte]) {
    val parts = internalName.split('/')
    val f = new io.VirtualFile(parts.last + ".class", parts.dropRight(1).mkString("/"))
    f.output
    getPackage(parts.dropRight(1)).classes += new ClassRep(Some(f), None)
  }

  def addJavaSource(internalName: String, content: String) {
    val parts = internalName.split('/')
    val f = new io.VirtualFile(parts.last + ".java", parts.dropRight(1).mkString("/"))
    getPackage(parts.dropRight(1)).classes += new ClassRep(None, Some(f))
  }

  def addSource(internalName: String, content: String) {
    val parts = internalName.split('/')
    val f = new io.VirtualFile(parts.last + ".scala", parts.dropRight(1).mkString("/"))
    getPackage(parts.dropRight(1)).classes += new ClassRep(None, Some(f))
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