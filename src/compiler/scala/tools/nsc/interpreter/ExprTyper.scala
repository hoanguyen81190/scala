/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.tools.nsc.ast.parser.Tokens.EOF

trait ExprTyper {
  val repl: IMain

  import repl._
  import global.{ reporter => _, Import => _, _ }
  import syntaxAnalyzer.{ UnitParser }

  object codeParser extends { val global: repl.global.type = repl.global } with CodeHandlers[Tree] {
    def applyRule[T](code: String, rule: UnitParser => T): T = {
      reporter.reset()
      val scanner = newUnitParser(code)
      val result  = rule(scanner)

      if (!reporter.hasErrors)
        scanner.accept(EOF)

      result
    }

    def defns(code: String) = stmts(code) collect { case x: DefTree => x }
    def expr(code: String)  = applyRule(code, _.expr())
    def stmts(code: String) = applyRule(code, _.templateStats())
    def stmt(code: String)  = stmts(code).last  // guaranteed nonempty
  }
  /** Parse a line into a sequence of trees. Returns None if the input is incomplete. */
  def parse(line: String): Option[List[Tree]] = {
    var isIncomplete = false
    reporter.withIncompleteHandler((_, _) => isIncomplete = true) {
      val trees = codeParser.stmts(line)
      if (reporter.hasErrors) Some(Nil)
      else if (isIncomplete) None
      else Some(trees)
    }
  }


}
