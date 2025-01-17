/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools
package partest

import scala.actors.Actor._
import scala.util.Properties.setProp
import scala.tools.nsc.io.{ Directory, Path => SPath }
import nsc.util.ClassPath
import util.PathResolver
import scala.tools.ant.sabbus.CompilationPathProperty

import java.io.File
import java.lang.reflect.Method

import org.apache.tools.ant.Task
import org.apache.tools.ant.types.{Path, Reference, FileSet}

class PartestTask extends Task with CompilationPathProperty {

  def addConfiguredPosTests(input: FileSet) {
    posFiles = Some(input)
  }

  def addConfiguredNegTests(input: FileSet) {
    negFiles = Some(input)
  }

  def addConfiguredRunTests(input: FileSet) {
    runFiles = Some(input)
  }

  def addConfiguredJvmTests(input: FileSet) {
    jvmFiles = Some(input)
  }

  def addConfiguredResidentTests(input: FileSet) {
    residentFiles = Some(input)
  }
  
  def addConfiguredBuildManagerTests(input: FileSet) {
    buildManagerFiles = Some(input)
  }
  
  def addConfiguredScalacheckTests(input: FileSet) {
    scalacheckFiles = Some(input)
  }

  def addConfiguredScriptTests(input: FileSet) {
    scriptFiles = Some(input)
  }

  def addConfiguredShootoutTests(input: FileSet) {
    shootoutFiles = Some(input)
  }

  def addConfiguredScalapTests(input: FileSet) {
    scalapFiles = Some(input)
  }

  def addConfiguredJribbleTests(input: FileSet) {
    jribbleFiles = Some(input)
  }
  
  def addConfiguredSpecializedTests(input: FileSet) {
    specializedFiles = Some(input)
  }

  def addConfiguredPresentationTests(input: FileSet) {
    presentationFiles = Some(input)
  }

  
  def setSrcDir(input: String) {
    srcDir = Some(input)
  }

  def setClasspath(input: Path) {
    if (classpath.isEmpty)
      classpath = Some(input)
    else
      classpath.get.append(input)
  }

  def createClasspath(): Path = {
    if (classpath.isEmpty) classpath = Some(new Path(getProject()))
    classpath.get.createPath()
  }
  
  def setClasspathref(input: Reference) {
    createClasspath().setRefid(input)
  }
  
  def setShowLog(input: Boolean) {
    showLog = input
  }
  
  def setShowDiff(input: Boolean) {
    showDiff = input
  }
  
  def setErrorOnFailed(input: Boolean) {
    errorOnFailed = input
  }
    
  def setJavaCmd(input: File) {
    javacmd = Some(input)
  }

  def setJavacCmd(input: File) {
    javaccmd = Some(input)
  }

  def setScalacOpts(opts: String) {
    scalacOpts = Some(opts)
  }

  def setTimeout(delay: String) {
    timeout = Some(delay)
  }

  def setDebug(input: Boolean) {
    debug = input
  }
  
  def setJUnitReportDir(input: File) {
    jUnitReportDir = Some(input)
  }

  private var classpath: Option[Path] = None
  private var srcDir: Option[String] = None
  private var javacmd: Option[File] = None
  private var javaccmd: Option[File] = None
  private var showDiff: Boolean = false
  private var showLog: Boolean = false
  private var runFailed: Boolean = false
  private var posFiles: Option[FileSet] = None
  private var negFiles: Option[FileSet] = None
  private var runFiles: Option[FileSet] = None
  private var jvmFiles: Option[FileSet] = None
  private var residentFiles: Option[FileSet] = None
  private var buildManagerFiles: Option[FileSet] = None
  private var scalacheckFiles: Option[FileSet] = None
  private var scriptFiles: Option[FileSet] = None
  private var shootoutFiles: Option[FileSet] = None
  private var scalapFiles: Option[FileSet] = None
  private var specializedFiles: Option[FileSet] = None
  private var presentationFiles: Option[FileSet] = None
  private var jribbleFiles: Option[FileSet] = None
  private var errorOnFailed: Boolean = false
  private var scalacOpts: Option[String] = None
  private var timeout: Option[String] = None
  private var jUnitReportDir: Option[File] = None
  private var debug = false

  def fileSetToDir(fs: FileSet) = Directory(fs getDir getProject)
  def fileSetToArray(fs: FileSet): Array[SPath] = {
    val root = fileSetToDir(fs)
    (fs getDirectoryScanner getProject).getIncludedFiles map (root / _)
  }

  private def getFiles(fileSet: Option[FileSet]): Array[File] = fileSet match {
    case None     => Array()
    case Some(fs) => fileSetToArray(fs) filterNot (_ hasExtension "log") map (_.jfile)
  }

  private def getFilesAndDirs(fileSet: Option[FileSet]): Array[File] = fileSet match {
    case None     => Array()
    case Some(fs) =>
      def shouldExclude(name: String) = (name endsWith ".obj") || (name startsWith ".")
      // println("----> " + fileSet)
    
      val fileTests = getFiles(Some(fs)) filterNot (x => shouldExclude(x.getName))
      val dirResult = getDirs(Some(fs))  filterNot (x => shouldExclude(x.getName))
      // println("dirs: " + dirResult.toList)
      // println("files: " + fileTests.toList)
      
      dirResult ++ fileTests
  }

  private def getDirs(fileSet: Option[FileSet]): Array[File] = fileSet match {
    case None     => Array()
    case Some(fs) =>
      def shouldExclude(name: String) = (name endsWith ".obj") || (name startsWith ".")
    
      val dirTests: Iterator[SPath] = fileSetToDir(fs).dirs filterNot (x => shouldExclude(x.name))
      val dirResult = dirTests.toList.toArray map (_.jfile)
      
      dirResult
  }


  private def getPosFiles          = getFilesAndDirs(posFiles)
  private def getNegFiles          = getFilesAndDirs(negFiles)
  private def getRunFiles          = getFilesAndDirs(runFiles)
  private def getJvmFiles          = getFilesAndDirs(jvmFiles)
  private def getResidentFiles     = getFiles(residentFiles)
  private def getBuildManagerFiles = getFilesAndDirs(buildManagerFiles)
  private def getScalacheckFiles   = getFilesAndDirs(scalacheckFiles)
  private def getScriptFiles       = getFiles(scriptFiles)
  private def getShootoutFiles     = getFiles(shootoutFiles)
  private def getScalapFiles       = getFiles(scalapFiles)
  private def getSpecializedFiles  = getFiles(specializedFiles)
  private def getPresentationFiles = getDirs(presentationFiles)
  private def getJribbleFiles       = getFilesAndDirs(jribbleFiles)

  override def execute() {
    if (isPartestDebug || debug) {
      setProp("partest.debug", "true")
      nest.NestUI._verbose = true
    }
    
    srcDir foreach (x => setProp("partest.srcdir", x))
    
    val classpath = this.compilationPath getOrElse sys.error("Mandatory attribute 'compilationPath' is not set.")
    
    val scalaLibrary = {
      (classpath.list map { fs => new File(fs) }) find { f =>
        f.getName match {
          case "scala-library.jar" => true
          case "library" if (f.getParentFile.getName == "classes") => true
          case _ => false
        }
      }
    } getOrElse sys.error("Provided classpath does not contain a Scala library.") 
    
    val antRunner = new scala.tools.partest.nest.AntRunner
    val antFileManager = antRunner.fileManager
    
    antFileManager.showDiff = showDiff
    antFileManager.showLog = showLog
    antFileManager.failed = runFailed
    antFileManager.CLASSPATH = ClassPath.join(classpath.list: _*)
    antFileManager.LATEST_LIB = scalaLibrary.getAbsolutePath
    
    javacmd foreach (x => antFileManager.JAVACMD = x.getAbsolutePath)
    javaccmd foreach (x => antFileManager.JAVAC_CMD = x.getAbsolutePath)
    scalacOpts foreach (antFileManager.SCALAC_OPTS = _)
    timeout foreach (antFileManager.timeout = _)
    
    type TFSet = (Array[File], String, String)
    val testFileSets = List(
      (getPosFiles, "pos", "Compiling files that are expected to build"),
      (getNegFiles, "neg", "Compiling files that are expected to fail"),
      (getRunFiles, "run", "Compiling and running files"),
      (getJvmFiles, "jvm", "Compiling and running files"),
      (getResidentFiles, "res", "Running resident compiler scenarii"),
      (getBuildManagerFiles, "buildmanager", "Running Build Manager scenarii"),
      (getScalacheckFiles, "scalacheck", "Running scalacheck tests"),
      (getScriptFiles, "script", "Running script files"),
      (getShootoutFiles, "shootout", "Running shootout tests"),
      (getScalapFiles, "scalap", "Running scalap tests"),
      (getSpecializedFiles, "specialized", "Running specialized files"),
      (getPresentationFiles, "presentation", "Running presentation compiler test files"),
      (getJribbleFiles, "scalap", "Running jribble tests")
    )
    
    def runSet(set: TFSet): (Int, Int, Iterable[String]) = {
      val (files, name, msg) = set
      if (files.isEmpty) (0, 0, List())
      else {
        log(msg)
        val results: Iterable[(String, Int)] = antRunner.reflectiveRunTestsForFiles(files, name)
        val (succs, fails) = resultsToStatistics(results)

        val failed: Iterable[String] = results collect {
          case (path, 1)    => path + " [FAILED]"
          case (path, 2)    => path + " [TIMOUT]"
        }

        // create JUnit Report xml files if directory was specified
        jUnitReportDir foreach { d =>
          d.mkdir
          
          val report = testReport(name, results, succs, fails)
          scala.xml.XML.save(d.getAbsolutePath+"/"+name+".xml", report)
        }
        
        (succs, fails, failed)
      }
    }

    val _results = testFileSets map runSet
    val allSuccesses = _results map (_._1) sum
    val allFailures = _results map (_._2) sum
    val allFailedPaths = _results flatMap (_._3)

    def f = if (errorOnFailed && allFailures > 0) (sys error _) else log(_: String)
    def s = if (allFailures > 1) "s" else ""
    val msg =
      if (allFailures > 0)
        "Test suite finished with %d case%s failing:\n".format(allFailures, s)+
        allFailedPaths.mkString("\n")
      else if (allSuccesses == 0) "There were no tests to run."
      else "Test suite finished with no failures."
    
    f(msg)
  }
  def oneResult(res: (String, Int)) =
    <testcase name={res._1}>{
  	  res._2 match {
  	    case 0 => scala.xml.NodeSeq.Empty
        case 1 => <failure message="Test failed"/>
        case 2 => <failure message="Test timed out"/>
  	  } 
  	}</testcase>
   
  def testReport(kind: String, results: Iterable[(String, Int)], succs: Int, fails: Int) =
    <testsuite name={kind} tests={(succs + fails).toString} failures={fails.toString}>
  	  <properties/>
  	  {
  	    results.map(oneResult(_))
  	  }
    </testsuite>
}
