/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package repl

import io.Path
import java.awt.{ Desktop => JDesktop }
import java.net.URI

object Desktop {
  def desktop = apply()
  def apply() = JDesktop.getDesktop()
  def isSupported = JDesktop.isDesktopSupported()
  
  def browse[T <% URI](uri: T)      = desktop.browse(uri)
  def edit[T <% Path](file: Path)   = desktop.edit(file.jfile)
  def mail[T <% URI](uri: T = null) = if (uri == null) desktop.mail() else desktop.mail(uri)
  def open[T <% Path](file: Path)   = desktop.open(file.jfile)
  def print[T <% Path](file: Path)  = desktop.print(file.jfile)
}
