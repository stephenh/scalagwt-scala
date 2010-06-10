/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package repl

import scala.tools.cmd.program.Tokens.fromScalaString

/** All kinds of string manipulations.
 */
trait Strings {  
  def ppsrc(code: String): String = {
    val buf = new StringBuilder
    var depth = 0

    def indent() = buf append ("  " * depth)
    def nl() = buf += '\n'
    
    def loop(xs: Seq[Char]): String = {
      if (xs.isEmpty)
        return buf.toString
      
      def next = xs.tail dropWhile (_.isWhitespace)
      
      def finish() = {
        nl()
        indent()
        loop(next)
      }
        
      xs.head match {
        case '{'  =>
          depth += 1
          buf += '{'
          finish()
        case '}'  =>
          nl()
          depth -= 1
          indent()
          buf += '}'
          loop(next)
        case '\n' =>
          finish()
        case x    =>
          buf += x
          loop(xs.tail)
      }
    }
    
    loop(code)
  }

  /** Heuristically strip interpreter wrapper prefixes
   *  from an interpreter output string.
   */
  def stripWrapperGunk(str: String): String = {
    val wrapregex = """(line[0-9]+\$object[$.])?(\$iw[$.])*"""
    val replvals = """\$replvals\."""
    str.replaceAll(wrapregex, "").replaceAll(replvals, "")
  }

  /** Truncate a string if it is longer than max. */
  def truncPrintString(str: String, max: Int): String = {
    val trailer = "..."
    
    if (max <= 0 || str.length <= max) str
    else str.substring(0, max-3) + trailer
  }
  
  /** Removes newlines and smashes a line of code down to 50 chars. */
  def shortCode(line: String) = truncPrintString(line.replaceAll("\n", " "), 50)
}

object Strings extends Strings { }