/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package repl

import java.io.File
import jline.{ ConsoleReader, ArgumentCompletor, History => JHistory }

/** Reads from the console using JLine */
class JLineReader(interpreter: Interpreter) extends InteractiveReader {
  def this() = this(null)
  
  val interactive = true
  lazy val consoleReader = new ReplConsoleReader init()
  override lazy val history = Some(History(consoleReader))
  override lazy val completion = Option(interpreter) map (x => new Completion(x))
  
  class ReplConsoleReader() extends ConsoleReader() {
    def init(): this.type = {
      this setHistory (History().jhistory)
      this setBellEnabled false 
      completion foreach { c =>
        this addCompletor c.jline
        this setAutoprintThreshhold 250
      }
      this
    }
  }      
  
  def readOneLine(prompt: String) = consoleReader readLine prompt
}

