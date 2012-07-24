/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stepan Koltsov
 */

package scala.tools.nsc
package interpreter

import java.io.{ BufferedReader }
import java.io.PrintWriter

/** Reads using standard JDK API */
class SimpleReader(
                    in: BufferedReader,
                    out: PrintWriter,
                    val interactive: Boolean)
{
  def readLine(prompt: String): String = {
    if (interactive) {
      out.print(prompt)
      out.flush()
    }
    in.readLine()
  }
}

object SimpleReader {
  def defaultIn  = Console.in
  def defaultOut = new PrintWriter(Console.out)

  def apply(): SimpleReader = new SimpleReader(defaultIn, defaultOut, true)
  def apply(in: BufferedReader = defaultIn, out: PrintWriter = defaultOut, interactive: Boolean = true): SimpleReader =
    new SimpleReader(in, out, interactive)
}