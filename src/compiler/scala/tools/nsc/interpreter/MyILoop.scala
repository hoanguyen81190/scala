/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Alexander Spoon
 */

package scala.tools.nsc
package interpreter

import Predef.{ println => _, _ }

/**
 *  @author Moez A. Abdel-Gawad
 *  @author  Lex Spoon
 *  @version 1.2
 */
class MyILoop(protected val out: JPrintWriter)
  extends AnyRef
{
  def this() = this(new JPrintWriter(Console.out, true))

  var in: InteractiveReader = _   // the input stream from which commands come
  var settings: Settings = _
  var intp: IMain = _

  /** Close the interpreter and set the var to null. */
  def closeInterpreter() {
    if (intp ne null) {
      intp.close()
      intp = null
    }
  }

  class MyILoopInterpreter extends IMain(settings, out) {
    outer =>

    override lazy val formatting = new Formatting {
      def prompt = MyILoop.this.prompt
    }

  }

  def createInterpreter() {
    intp = new MyILoopInterpreter
  }

  private var currentPrompt = Properties.shellPromptString
  def prompt = currentPrompt


  def loop() {
    def readOneLine() = {
      out.flush()
      in readLine prompt
    }
    // return false if repl should exit
    def processLine(line: String): Boolean = {
      if (line eq null) false               // assume null means EOF
      intp.interpret(line)
      true
    }
    def innerLoop() {
      if ( processLine(readOneLine()) )
        innerLoop()
    }
    innerLoop()
  }


  def process(settings: Settings): Boolean = {
    this.settings = settings
    createInterpreter()

    in = SimpleReader()

    try loop()
    catch AbstractOrMissingHandler()
    finally closeInterpreter()

    true
  }
}
