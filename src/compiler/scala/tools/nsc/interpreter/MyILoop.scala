package scala.tools.nsc
package interpreter

import Predef.{ println => _, _ }
import java.io.PrintWriter

class MyILoop(protected val out: PrintWriter)
  extends AnyRef
{
  def this() = this(new PrintWriter(Console.out, true))

  var in: SimpleReader = _   // the input stream from which commands come
  var settings: Settings = _
  var intp: IMain = _

  def closeInterpreter() {
    if (intp ne null) {
      intp.close()
      intp = null
    }
  }

  def createInterpreter() {
    intp = new IMain(settings, out)
  }

  def prompt = Properties.shellPromptString

  def loop() {
    def readOneLine() = {
      out.flush()
      in readLine prompt
    }

    def innerLoop() {
      if ( intp.interpret(readOneLine()) )
        innerLoop()
    }
    innerLoop()
  }

  def process(settings: Settings): Boolean = {
    this.settings = settings
    createInterpreter()

    in = SimpleReader()

    loop()
    closeInterpreter()

    true
  }
}