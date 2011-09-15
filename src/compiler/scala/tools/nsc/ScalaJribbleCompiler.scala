package scala.tools.nsc

import com.google.jribble.unit.JribbleCompiler
import com.google.jribble.unit.JribbleUnitResult
import scala.tools.nsc.io.VirtualDirectory

class ScalaJribbleCompiler extends JribbleCompiler {

  val output = new VirtualDirectory("(memory)", None)
  val settingArgs = List(
    "-g:notailcalls",
    "-Xplugin-classes:scala.tools.factorymanifests.FactoryManifestsPlugin")

  def addClassBytes(internalName: String, classBytes: Array[Byte]) {
  }

  def addJavaSource(internalName: String, content: String) {
  }

  def addSource(internalName: String, content: String) {
  }

  /** Invokes the compiler and returns the new units. */
  def compile(): java.util.Collection[JribbleUnitResult] = {
    val ss = new Settings(error);
    ss.outputDirs setSingleOutput virtualDirectory
    val command = new CompilerCommand(settingArgs, ss);
    val compiler = new Global(command.settings, null) {
      override lazy val platform: ThisPlatform = {
        new { val global: Global.this.type = Global.this } with backend.JribblePlatform {
          lazy val classPath = null // special source path here
        }
      }
    }
    val run = new compiler.Run();
    null
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