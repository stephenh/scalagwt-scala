/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package repl

import scala.collection.mutable.HashMap

trait TypeCache {
  self: Interpreter =>
  
  import compiler._
  
  def atTyper[T](op: => T): T = atPhase(currentRun.typerPhase.next)(op)

  type Decl = Req#Decl
  
  val reqHistory    = new ListBuffer[Req]
  def origHistory   = reqHistory.toList map (_.orig)
  def declHistory   = reqHistory.toList flatMap (_.decls)
  // def cleanHistory  = reqHistory.toList filterNot (_.isSynthetic)
  // def cleanDeclHist = cleanHistory flatMap (_.decls)
  // def reqHandlers   = cleanHistory flatMap (_.handlers)
  def reqHandlers   = reqHistory flatMap (_.handlers)
  def latestTree    = reqHandlers.lastOption map (_.member)
  
  def allReqAndHandlers = reqHistory.toList flatMap (req => req.handlers map (req -> _))
  def allHandlers = reqHistory.toList flatMap (_.handlers)
  
  def past(req: Req)    = reqHistory.toList takeWhile (_ ne this)
  def future(req: Req)  = reqHistory.toList drop (past(req).size + 1)

  def lookBack(req: Req)      = past(req).reverse
  def lookBackFlat(req: Req)  = declHistory takeWhile (_.req ne req) reverse

  def latestWhich(req: Req)(cond: Decl => Boolean) = lookBackFlat(req) find cond
  def prevWhich(req: Req)(cond: Decl => Boolean) = lookBackFlat(req) filter cond
  
  def recordReq(req: Req) = reqHistory += req
  
  private def all[T](f: Req => List[T]): Set[T] = (reqHistory flatMap f).toSet
  def allNames = all(_.declaredNames)
  
  // def allPrevNames  = all(_.declaredNames)
  // def allPrevTrees  = all(_.trees)
  // def allPrevSyms   = all(_.syms)
  // def allPrevTypes  = all(_.types)

  // def shadowCheck = shadows match {
  //   case Nil  => ()
  //   case xs   =>
  //     val decl = declarations find (_.name == xs.head._2.name) get;
  //     cleanprintln(decl.sym + " shadows " + xs.size + " earlier definitions.")
  //   
  //     for (((req, decl2), index) <- xs.zipWithIndex) {
  //       val fmt = if (decl.tpe <:< decl2.tpe) "%s <:< %s" else "!(%s <:< %s)"
  //       val formatted = fmt.format(decl.typeString, decl2.typeString)
  //       cleanprintln((index + 1) + ") " + formatted)
  //     }
  // } 
  // 
  
  def latest(name: Name)    = declHistory.reverse find (_.name == name)
  def treeOf(name: Name)    = latest(name) map (_.tree)
  def tpeOf(name: Name)     = latest(name) map (_.tpe)
  def symOf(name: Name)     = latest(name) map (_.sym)
  
  def runOf(name: Name)     = reqOf(name) map (_.objectRun)
  def reqOf(name: Name)     = reqHistory.reverse find (_.declaredNames contains name)
}


