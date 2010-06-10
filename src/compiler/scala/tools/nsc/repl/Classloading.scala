/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package repl

import ScalaClassLoader.URLClassLoader
import scala.tools.util.PathResolver

trait Classloading {
  self: Interpreter =>
  
  /** Class objects */
  // def classForName(name: String): Option[Class[_]] =
  //   try Some(Class forName name)
  //   catch { case _: ClassNotFoundException | _: SecurityException => None }
  
  /* A single class loader is used for all commands interpreted by this Interpreter.
     It would also be possible to create a new class loader for each command
     to interpret.  The advantages of the current approach are:

       - Expressions are only evaluated one time.  This is especially
         significant for I/O, e.g. "val x = Console.readLine"

     The main disadvantage is:

       - Objects, classes, and methods cannot be rebound.  Instead, definitions
         shadow the old ones, and old code objects refer to the old
         definitions.
  */
  private var _classLoader: AbstractFileClassLoader = null
  def resetClassLoader() = _classLoader = makeClassLoader()
  def classLoader: AbstractFileClassLoader = {
    if (_classLoader == null)
      resetClassLoader()
    
    _classLoader
  }
  private def makeClassLoader(): AbstractFileClassLoader = {
    val parent =
      if (parentClassLoader == null)  ScalaClassLoader fromURLs compilerClasspath
      else                            new URLClassLoader(compilerClasspath, parentClassLoader)

    new AbstractFileClassLoader(virtualDirectory, parent)
  }
  def loadByName(s: String): Class[_] = (classLoader tryToInitializeClass s).get
  
  protected def parentClassLoader: ClassLoader = this.getClass.getClassLoader()
  def getInterpreterClassLoader() = classLoader

  // Set the current Java "context" class loader to this interpreter's class loader
  def setContextClassLoader() = classLoader.setAsContext()
}