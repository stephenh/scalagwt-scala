/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Alexander Spoon
 */

package scala.tools.nsc
package repl

import util.returning

/** Settings for the interpreter
 *
 * @version 1.0
 * @author Lex Spoon, 2007/3/24
 **/
class REPLSettings(val out: PrintWriter) extends settings.MutableSettings(out println _) {
  /** A list of paths where :load should look */
  val loadPath = PathSetting("-loadpath", "path", "Specify where :load should look for files", ".")  

  /** The maximum length of toString to use when printing the result
   *  of an evaluation.  0 means no maximum.  If a printout requires
   *  more than this number of characters, then the printout is
   *  truncated.
   */
  val maxPrintString = IntSetting("-maxPrintString", "Max length of toString to print in repl", 800, None, _ => None)
  
  /** The maximum number of completion candidates to print for tab
   *  completion without requiring confirmation.
   */
  val maxAutoprintCompletion = IntSetting("-maxAutoprintCompletion", "Max number of completion candidates", 250, None, _ => None)
  
  /** String unwrapping can be disabled if it is causing issues.
   *  Settings this to false means you will see Strings like "$iw.$iw.".
   */
  val rawStrings = BooleanSetting("-raw-strings", "Show raw Strings in the repl.")
  
  def setFromRepl[T: Manifest](lhs: Setting, rhs: T): Boolean = {    
    lhs match {
      case x: BooleanSetting    =>
        manifest[T] == manifest[Boolean] && {
          x.value = rhs.asInstanceOf[Boolean]
          true
        }
      case x: ColonSetting      => (
        if (manifest[T] == manifest[List[String]]) x.tryToSetColon(rhs.asInstanceOf[List[String]])
        else x.tryToSetColon(List(rhs.toString))
      ).isDefined
      case _                    => lhs.tryToSet(List(rhs.toString)).isDefined
    }
  }
  
  def unsetFromRepl[T: Manifest](lhs: Setting): Boolean = {
    lhs.unsetValue()
    true
  }
}

object REPLSettings {
  def defaultWriter = new NewLinePrintWriter(new ConsoleWriter, true)

  def apply(): REPLSettings = new REPLSettings(defaultWriter)
  def apply(out: PrintWriter): REPLSettings = new REPLSettings(out)
  def apply(settings: Settings): REPLSettings = apply(settings, defaultWriter)
  def apply(settings: Settings, out: PrintWriter): REPLSettings = returning(new REPLSettings(out))(settings copyInto _)
}

