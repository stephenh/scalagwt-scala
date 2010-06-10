/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */
 
package scala.tools.nsc
package repl

/** An interface for objects which are aware of tab completion and
 *  will supply their own candidates and resolve their own paths.
 */
trait CompletionAware {
  type ExecResult
  private def toExec(x: Any): ExecResult = x.asInstanceOf[ExecResult]
  
  /** The delimiters which are meaningful when this CompletionAware
   *  object is in control.
   */
  // TODO
  // def delimiters(): List[Char] = List('.')

  /** The complete list of unqualified Strings to which this
   *  object will complete.
   */
  def completions(verbosity: Int): List[String]
  
  /** Default filter to apply to completions.
   */
  def filterNotFunction(s: String): Boolean = false
  
  /** Default sort.
   */
  def sortFunction(s1: String, s2: String): Boolean = s1 < s2
  
  /** Default map.
   */
  def mapFunction(s: String) = NameTransformer decode s
  
  /** The next completor in the chain.
   */
  def follow(id: String): Option[CompletionAware] = None
  
  /** What to return if this completion is given as a command.  It
   *  returns None by default, which means to allow the repl to interpret
   *  the line normally.  Returning Some(_) means the line will never
   *  reach the scala interpreter.
   */
  def execute(id: String): Option[ExecResult] = None

  /** A list of useful information regarding a specific uniquely
   *  identified completion.  This is specifically written for the
   *  following situation, but should be useful elsewhere too:
   *
   *    x.y.z.methodName<tab>
   *
   *  If "methodName" is among z's completions, and verbosity > 0
   *  indicating tab has been pressed twice consecutively, then we
   *  call alternativesFor and show a list of overloaded method
   *  signatures.
   */
  def alternativesFor(id: String): List[String] = Nil
  
  /** Given string 'buf', return a list of all the strings
   *  to which it can complete.  This may involve delegating
   *  to other CompletionAware objects.
   */
  def completionsFor(parsed: Parsed): List[String] = {
    import parsed._
    
    val comps = completions(verbosity) filter (_ startsWith buffer)
    val results =    
      if (isEmpty) comps
      else if (isUnqualified && !isLastDelimiter) {
        if (verbosity > 0 && (comps contains buffer)) alternativesFor(buffer)
        else comps
      }
      else follow(bufferHead) map (_ completionsFor bufferTail) getOrElse Nil
  
    results filterNot filterNotFunction map mapFunction sortWith (sortFunction _)
  }
  
  /** TODO - unify this and completionsFor under a common traverser.
   */
  def executionFor(parsed: Parsed): Option[ExecResult] = {
    import parsed._
    
    if (isUnqualified && !isLastDelimiter && (completions(verbosity) contains buffer)) execute(buffer)
    else if (!isQualified) None
    else follow(bufferHead) flatMap (_ executionFor bufferTail) map toExec
  }
}

trait ExecCompletionAware[T] extends CompletionAware {
  type ExecResult = T
  // implicit def execManifest: Manifest[T]
}

object CompletionAware {
  val Empty = new CompletionAware { def completions(verbosity: Int) = Nil }

  def unapply(that: Any): Option[CompletionAware] = that match {
    case x: CompletionAware => Some((x))
    case _                  => None
  }
  
  /** Create a CompletionAware object from the given functions.
   *  The first should generate the list of completions whenever queried,
   *  and the second should return Some(CompletionAware) object if
   *  subcompletions are possible.
   */
  def apply(terms: () => List[Any], followFunction: String => Option[CompletionAware]): CompletionAware =
    new CompletionAware {
      def completions = terms() map (_.toString)
      def completions(verbosity: Int) = completions
      override def follow(id: String) = followFunction(id)
    }

  /** Convenience factories.
   */
  def apply(terms: () => List[Any]): CompletionAware = apply(terms, _ => None)
  def apply(map: collection.Map[String, CompletionAware]): CompletionAware =
    apply(() => map.keys.toList, map.get _)
}

