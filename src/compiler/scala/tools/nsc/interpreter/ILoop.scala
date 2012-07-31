package scala.tools.nsc
package interpreter

import java.io.{InputStreamReader, BufferedReader, PrintWriter}

class ILoop(protected val out: PrintWriter)
{
  def this() = this(new PrintWriter(System.out, true))

  var in = new BufferedReader(new InputStreamReader(System.in))   // the input stream from which commands come
  var intp: IMain = _

  def closeInterpreter() {
    if (intp ne null) {
      intp.close()
      intp = null
    }
  }

  def createInterpreter() {
    intp = new IMain(out)
  }

  def loop() {
    def readOneLine() = {
      out.print("scala> ")
      out.flush()
      in readLine
    }

    def innerLoop() {
      if ( intp.interpret(readOneLine()) )
        innerLoop()
    }
    innerLoop()
  }

  def process : Boolean = {
    createInterpreter()

    loop()
    closeInterpreter()
    true
  }
}

object ILoop {
  def main(args: Array[String]) {
    new ILoop process
  }
}