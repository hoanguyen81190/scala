/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

/** This is for name logic which is independent of the compiler (notice there's no Global.)
 *  That includes at least generating, metaquoting, mangling, and unmangling.
 */
trait Naming {
  trait SessionNames {
    // All values are configurable by passing e.g. -Dscala.repl.name.read=XXX
    final def propOr(name: String): String = propOr(name, "$" + name)
    final def propOr(name: String, default: String): String =
      sys.props.getOrElse("scala.repl.name." + name, default)

    // Prefixes used in repl machinery.  Default to $line, $read, etc.
    def line   = propOr("line")
    def read   = propOr("read")
    def eval   = propOr("eval")
    def print  = propOr("print")
    def result = propOr("result")

  }
  lazy val sessionNames: SessionNames = new SessionNames { }


  val freshLineId            = {
    var x = 0
    () => { x += 1 ; x }
  }

//  def freshInternalVarName() = internalVar()
}
