/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */
 
package scala.tools.nsc

import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream, PrintWriter, StringWriter }
import java.security.AccessControlException

package object util {  
  /** Apply a function and return the passed value */
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }

  def ifPermitted[T](body: => T): Option[T] =
    try   { Some(body) }
    catch { case _: SecurityException | _: AccessControlException => None }
    
  def ifPermittedOr[T](zero: T)(body: => T): T = ifPermitted(body) getOrElse zero
  
  /** All living threads. */
  def allThreads(): List[Thread] = {
    val num = Thread.activeCount()
    val tarray = new Array[Thread](num)
    val got = Thread.enumerate(tarray)
    
    tarray take got toList
  }
  
  /** Execute code and then wait for all Threads created during its
   *  execution to complete.
   */
  def waitingForThreads[T](body: => T) = {
    val ts1 = allThreads()
    val result = body
    val ts2 = allThreads()
    val newThreads = (ts2.toSet -- ts1) filterNot (_.isDaemon())
    
    newThreads foreach (_.join())
    result
  }
  
  /** Given function and block of code, evaluates code block,
   *  calls function with milliseconds elapsed, and returns block result.
   */
  def timed[T](f: Long => Unit)(body: => T): T = {
    val start = System.currentTimeMillis
    val result = body
    val end = System.currentTimeMillis
    
    f(end - start)
    result
  }

  /** Generate a string using a routine that wants to write on a stream. */
  def stringFromWriter(writer: PrintWriter => Unit): String = {
    val stringWriter = new StringWriter()
    val stream = new NewLinePrintWriter(stringWriter)
    writer(stream)
    stream.close()
    stringWriter.toString
  }
  def stringFromStream(stream: OutputStream => Unit): String = {
    val bs = new ByteArrayOutputStream()
    val ps = new PrintStream(bs)
    stream(ps)
    ps.close()
    bs.toString()
  }
}
