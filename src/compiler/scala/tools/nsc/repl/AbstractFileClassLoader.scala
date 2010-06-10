/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.nsc
package repl

/**
 * A class loader that loads files from an AbstractFile.
 * 
 * @author Lex Spoon
 */
class AbstractFileClassLoader(root: AbstractFile, parent: ClassLoader)
    extends ClassLoader(parent)
    with ScalaClassLoader
{
  // override def getBytesForClass(name: String): Array[Byte] = classes.getOrElseUpdate(name, {
  override def getBytesForClass(name: String): Array[Byte] = {
    def default = super.getBytesForClass(name)
    
    var file: AbstractFile = root
    val pathParts = name.split("[./]").toList
    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, true)
      if (file == null)
        return default
    }
    file = file.lookupName(pathParts.last+".class", false)
    if (file == null) default else file.toByteArray
  }
  // })

  override def findClass(name: String): Class[_] = {
    val bytes = getBytesForClass(name)
    defineClass(name, bytes, 0, bytes.length)
  }
}
