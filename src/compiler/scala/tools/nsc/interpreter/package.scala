/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc.interpreter

package object interpreter {
  type JFile = java.io.File
  type JClass = java.lang.Class[_]
  type JList[T] = java.util.List[T]
  type JCollection[T] = java.util.Collection[T]
  type JPrintWriter = java.io.PrintWriter
  type InputStream = java.io.InputStream
  type OutputStream = java.io.OutputStream
}
