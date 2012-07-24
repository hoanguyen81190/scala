/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Lex Spoon
 */

package scala.tools.nsc

import interpreter.{MyILoop}

object MyREPL {

  def errorFn(str: String): Boolean = {
    Console.err println str
    false
  }
  def main(args: Array[String]) {
    val command = new GenericRunnerCommand(args.toList, (x: String) => errorFn(x))
    import command.{ settings}

    Right(new MyILoop process settings)
  }
}
