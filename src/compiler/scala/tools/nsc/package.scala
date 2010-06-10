package scala.tools

package object nsc {
  type Settings           = scala.tools.nsc.settings.MutableSettings
  type Interpreter        = scala.tools.nsc.repl.Interpreter
  type InterpreterLoop    = scala.tools.nsc.repl.InterpreterLoop

  val MainGenericRunner   = scala.tools.nsc.repl.Main
}

