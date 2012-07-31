/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.tools.nsc

import interpreter.{ILoop}

object MyREPL {
  def main(args: Array[String]) {
    new ILoop process
  }
}
