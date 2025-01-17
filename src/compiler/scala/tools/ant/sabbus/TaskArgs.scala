/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.tools.ant.sabbus

import java.io.File
import org.apache.tools.ant.Task
import org.apache.tools.ant.types.{Path, Reference}

trait CompilationPathProperty {
  this: Task =>
  
  protected var compilationPath: Option[Path] = None
  
  def setCompilationPath(input: Path) {
    if (compilationPath.isEmpty) compilationPath = Some(input)
    else compilationPath.get.append(input)
  }

  def createCompilationPath: Path = {
    if (compilationPath.isEmpty) compilationPath = Some(new Path(getProject()))
    compilationPath.get.createPath()
  }

  def setCompilationPathRef(input: Reference) {
    createCompilationPath.setRefid(input)
  }
}

trait TaskArgs extends CompilationPathProperty {
  this: Task =>
  
  def setId(input: String) {
    id = Some(input)
  }
  
  def setParams(input: String) {
    params = params match {
      case None => Some(input)
      case Some(ps) => Some(ps + " " + input)
    }
  }
  
  def setTarget(input: String) {
    compTarget = Some(input)
  }

  def setSrcPath(input: Path) {
    if (sourcePath.isEmpty) sourcePath = Some(input)
    else sourcePath.get.append(input)
  }

  def createSrcPath: Path = {
    if (sourcePath.isEmpty) sourcePath = Some(new Path(getProject()))
    sourcePath.get.createPath()
  }

  def setSrcPathRef(input: Reference) {
    createSrcPath.setRefid(input)
  }

  def setCompilerPath(input: Path) {
    if (compilerPath.isEmpty) compilerPath = Some(input)
    else compilerPath.get.append(input)
  }

  def createCompilerPath: Path = {
    if (compilerPath.isEmpty) compilerPath = Some(new Path(getProject()))
    compilerPath.get.createPath()
  }

  def setCompilerPathRef(input: Reference) {
    createCompilerPath.setRefid(input)
  }
  
  def setDestdir(input: File) {
    destinationDir = Some(input)
  }
  
  protected var id: Option[String] = None
  protected var params: Option[String] = None
  protected var compTarget: Option[String] = None
  protected var sourcePath: Option[Path] = None
  protected var compilerPath: Option[Path] = None
  protected var destinationDir: Option[File] = None
  
  def isMSIL = compTarget exists (_ == "msil")
  def isJribble = compTarget exists (_ == "jribble")
}
